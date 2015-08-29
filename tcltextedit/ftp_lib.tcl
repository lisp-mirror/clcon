#?-FTP_package
#?
#?   tcl FTP package -- 
# 
#   Required:   tcl8.0
#
#?  Created:	12/96 
#?  Changed:    05/98                            
#?  Version:    1.02
#
#   core ftp support: 	FTP::Open <server> <user> <passwd>
#		     	FTP::Close
#		    	FTP::Cd <directory>
#			FTP::Pwd
#			FTP::Type <ascii|binary>	
#			FTP::List <directory>
#			FTP::NList <directory>
#			FTP::ModTime <from> <to>
#			FTP::Delete <file>
#			FTP::Rename <from> <to>
#			FTP::Put <local> <?remote?>
#			FTP::Get <remote> <?local?>
#			FTP::Reget <remote> <?local?>
#			FTP::Newer <file>
#			FTP::MkDir <directory>
#			FTP::RmDir <directory>
#			FTP::Quote <arg1> <arg2> ...
#?
#?   Copyright (C) 1996-1998, Steffen Traeger
#?	EMAIL:	Steffen.Traeger@t-online.de
#?	URL:	http://home.t-online.de/home/Steffen.Traeger
#?
#?   This package is free software; you can redistribute it and/or 
#?   modify it. 
#?   This library is distributed in the hope that it will be useful,
#?   but WITHOUT ANY WARRANTY; without even the implied warranty of
#?   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#?
#?  The description of the procedures are partly exerpted from RFC 959.
#?   For more details see also RFC 959, RFC 1123, RFC 1579.
#?
########################################################################

package provide FTP [lindex {$Revision: 1.2 $} 1]

namespace eval FTP {

namespace export DisplayMsg Open Close Cd Pwd Type List NList ModTime\
		 Delete Rename Put Get Reget Newer Quote MkDir RmDir 
	
	
#############################################################################
#
# DisplayMsg --
#
# This is a simple procedure to display any messages on screen.
# It must be overwritten by users source code in the form:
# (exported)
#
#	namespace FTP {
#		proc DisplayMsg {msg} {
#			......
#		}
#	}
#
# Arguments:
# msg - 		message string
# state -		different states {normal, data, control, error}
#
proc DisplayMsg {msg {state ""}} {

#	switch $state {
#	  data		{puts $msg}
#	  control	{puts $msg}
#	  error		{puts "ERROR: $msg"}
#	  default 	{puts $msg}
#	}
}

#############################################################################
#
# Timeout --
#
# Handle timeouts
# 
# Arguments:
#  -
#
proc Timeout {} {
variable ftp
upvar #0 finished state

        after cancel $ftp(Wait)
	set state(control) 1

	DisplayMsg "Timeout of control connection after $ftp(Timeout) sec.!" error

}

#############################################################################
#
# WaitOrTimeout --
#
# Blocks the running procedure and waits for a variable of the transaction 
# to complete. It continues processing procedure until a procedure or 
# StateHandler sets the value of variable "finished". 
# If a connection hangs the variable is setting instead of by this procedure after 
# specified seconds in $ftp(Timeout).
#  
# 
# Arguments:
#  -		
#

proc WaitOrTimeout {} {
variable ftp
upvar #0 finished state

	set retvar 1

	if {[info exist state(control)]} {

		set ftp(Wait) [after [expr $ftp(Timeout) * 1000] [namespace current]::Timeout]

		vwait finished(control)
		set retvar $state(control)
	}

	return $retvar
}

#############################################################################
#
# WaitComplete --
#
# Transaction completed.
# Cancel execution of the delayed command declared in procedure WaitOrTimeout.
# 
# Arguments:
# value -	result of the transaction
#			0 ... Error
#			1 ... OK
#

proc WaitComplete {value} {
variable ftp
upvar #0 finished state

	if {[info exist state(data)]} {
		vwait finished(data)
	}

	after cancel $ftp(Wait)
	set state(control) $value
}

#############################################################################
#
# PutsCtrlSocket --
#
# Puts then specified command to control socket,
# if ftp(VERBOSE) is set than it logs via DisplayMsg
# 
# Arguments:
# command - 		ftp command
#

proc PutsCtrlSock {{command ""}} {
variable ftp 
variable VERBOSE
	
	if {$VERBOSE} {
		DisplayMsg "---> $command"
	}

	puts $ftp(CtrlSock) $command
	flush $ftp(CtrlSock)


}

#############################################################################
#
# StateHandler --
#
# Implements a finite state handler and a fileevent handler
# for the control channel
# 
# Arguments:
# sock - 		socket name
#			If called from a procedure than this argument is empty.
# 			If called from a fileevent than this argument contains
#			the socket channel identifier.

proc StateHandler {{sock ""}} {
variable ftp
variable DEBUG 

	# there is no socket (and no channel to get) if called from a procedure
	set rc "   "

	if { $sock != "" } {

		set number [gets $sock buffer]

		if { $number > 0 } {

			# get return code, check for multi-line text
			regexp "(^\[0-9\]+)( |-)?(.*)$" $buffer all rc multi_line

			set msg "C: $buffer"
			
			# multi-line format detected ("-"), get all the lines
			# until the real return code
			while { $multi_line == "-"  } {
				set number [gets $sock buffer]	
				if { $number > 0 } {
					append msg \n "C: $buffer"
					regexp "(^\[0-9\]+)( |-)?(.*)$" $buffer all rc multi_line
				}
			}
		} elseif [eof $ftp(CtrlSock)] {
			# remote server has closed control connection
			# kill control socket, unset State to disable all following command
			set rc 421
			DisplayMsg "C: 421 Service not available, closing control connection." control
			DisplayMsg "Service not available!" error
			catch {close $ftp(CtrlSock)}
			catch {close $ftp(DummySock)}
			catch {close $ftp(SourceCI)}
			catch {close $ftp(DestCI)}
			WaitComplete 0
			unset ftp(State)
			return
		}
		
	} 
	
	if {$DEBUG} {
		DisplayMsg "-> rc=\"$rc\"\n-> state=\"$ftp(State)\""
	}
	
	# system status replay
	if {$rc == "211"} {return}

	# use only the first digit 
	regexp "^\[0-9\]?" $rc rc
	
 	switch -- $ftp(State) {
	
		user	{ 
			  switch $rc {
			    2       {
			    	       PutsCtrlSock "USER $ftp(User)"
			               set ftp(State) passwd
			            }
			    default {
				       set errmsg "Error connecting!"
				       set complete_with 0
			            }
			  }
			}

		passwd	{
			  switch $rc {
			    2       {
				       set complete_with 1
			            }
			    3       {
			  	       PutsCtrlSock "PASS $ftp(Passwd)"
			  	       set ftp(State) connect
			            }
			    default {
				       set errmsg "Error connecting!"
				       set complete_with 0
			            }
			  }
			}

		connect	{
			  switch $rc {
			    2       {
				       set complete_with 1
			            }
			    default {
				       set errmsg "Error connecting!"
				       set complete_with 0
			            }
			  }
			}

		quit	{
		    	   PutsCtrlSock "QUIT"
			   set ftp(State) quit_sent
			}

		quit_sent {
			  switch $rc {
			    2       {
			               set complete_with 1
			            }
			    default {
				       set errmsg "Error disconnecting!"
				       set complete_with 0
			            }
			  }
			}
		
		abort	{
		    	   PutsCtrlSock "ABOR"
			   set ftp(State) abort_sent
			}

		abort_sent {
			  switch $rc {
			    2       {
			               set complete_with 1
			            }
			    default {
				       set errmsg "Error aborting!"
				       set complete_with 0
			            }
			  }
			}
		
		quote	{
		    	   PutsCtrlSock $ftp(Cmd)
			   set ftp(State) quote_sent
			}

		quote_sent {
			  switch $rc {
			    2       {
			               set complete_with 1
			            }
			    default {
				       set errmsg "Command not understood!"
				       set complete_with 0
			            }
			  }
			}
		
		type	{
		  	  if { $ftp(Type) == "ascii" } {
			  	PutsCtrlSock "TYPE A"
			  } else {
			  	PutsCtrlSock "TYPE I"
			  }
  		  	  set ftp(State) type_sent
			}
			
		type_sent {
			  switch $rc {
			    2       {
				       set complete_with 1
			            }
			    default {
				       set errmsg "Error setting type \"$ftp(Type)\"!"
				       set complete_with 0
			            }
			  }
			}
	
		nlist	{
			  if {[OpenDataConn]} {
			    	PutsCtrlSock "PORT $ftp(LocalAddr),$ftp(DataPort)"
			  	set ftp(State) nlist_open
			  } else {
				set errmsg "Error setting port!"
			  }
			  
			}
			
		nlist_open {
			  switch $rc {
			    2       {
			               PutsCtrlSock "NLST$ftp(Dir)"
			  	       set ftp(State) list_sent
			            }
			    default {
				       set errmsg "Error setting port!"
				       set complete_with 0
			            }
			  }
			}
	
		list	{
			  if {[OpenDataConn]} {
			    	PutsCtrlSock "PORT $ftp(LocalAddr),$ftp(DataPort)"
			  	set ftp(State) list_open
			  } else {
				set errmsg "Error setting port!"
			  }
			  
			}
			
		list_open {
			  switch $rc {
			    2       {
			               PutsCtrlSock "LIST$ftp(Dir)"
			  	       set ftp(State) list_sent
			            }
			    default {
				       set errmsg "Error setting port!"
				       set complete_with 0
			            }
			  }
			}
			
		list_sent	{
			  switch $rc {
			    1       {
			               set ftp(State) list_close
			            }
			    default {
				       set errmsg "Requested action not taken!"
				       set complete_with 0
			            }
			  }
			}

		list_close {
			  switch $rc {
			    2     {
			               set complete_with 1
			            }
			    default {
				       set errmsg "Error receiving list!"
				       set complete_with 0
			            }
			  }
			}
									
		modtime {
			  PutsCtrlSock "MDTM $ftp(File)"
  		  	  set ftp(State) modtime_sent
			}

		modtime_sent {
			  switch $rc {
			    2       {
            			       regexp "^\[0-9\]+ (.*)$" $buffer all ftp(DateTime)
			               set complete_with 1
			            }
			    default {
				       set errmsg "Error getting modification time!"
				       set complete_with 0
			            }
			  }
			}

		pwd	{
			   PutsCtrlSock "PWD"
  		  	   set ftp(State) pwd_sent
			}
			
		pwd_sent {
			  switch $rc {
			    2       {
            			       regexp "^.*\"(.*)\"" $buffer temp ftp(Dir)
			               set complete_with 1
			            }
			    default {
				       set errmsg "Error getting working dir!"
				       set complete_with 0
			            }
			  }
			}

		cd	{
			   PutsCtrlSock "CWD $ftp(Dir)"
  		  	   set ftp(State) cd_sent
			}
			
		cd_sent {
			  switch $rc {
			    2       {
			               set complete_with 1
			            }
			    default {
				       set errmsg "Error changing to \"$ftp(Dir)\"!"
				       set complete_with 0
				     }
			  }
			}
			
		mkdir	{
			  PutsCtrlSock "MKD $ftp(Dir)"
  		  	  set ftp(State) mkdir_sent
			}
			
		mkdir_sent {
			  switch $rc {
			    2       {
			               set complete_with 1
			            }
			    default {
				       set errmsg "Error making dir \"$ftp(Dir)\"!"
				       set complete_with 0
				     }
			  }
			}
			
		rmdir	{
			  PutsCtrlSock "RMD $ftp(Dir)"
  		  	  set ftp(State) rmdir_sent
			}
			
		rmdir_sent {
			  switch $rc {
			    2       {
			               set complete_with 1
			            }
			    default {
				       set errmsg "Error removing dir \"$ftp(Dir)\"!"
				       set complete_with 0
				     }
			  }
			}
										
		delete	{
			  PutsCtrlSock "DELE $ftp(File)"
  		  	  set ftp(State) delete_sent
			}
			
		delete_sent {
			  switch $rc {
			    2       {
			               set complete_with 1
			            }
			    default {
				       set errmsg "Error deleting file \"$ftp(File)\"!"
				       set complete_with 0
				     }
			  }
			}
			
		rename	{
			  PutsCtrlSock "RNFR $ftp(RenameFrom)"
  		  	  set ftp(State) rename_to
			}
			
		rename_to {
			  switch $rc {
			    3       {
			  	       PutsCtrlSock "RNTO $ftp(RenameTo)"
  		  	  	       set ftp(State) rename_sent
			            }
			    default {
				       set errmsg "Error renaming file \"$ftp(RenameFrom)\"!"
				       set complete_with 0
				     }
			  }
			}

		rename_sent {
			  switch $rc {
			    2     {
			               set complete_with 1
			            }
			    default {
				       set errmsg "Error renaming file \"$ftp(RenameFrom)\"!"
				       set complete_with 0
				     }
			  }
			}
			
		put 	{
			  if {[OpenDataConn]} {
			    	PutsCtrlSock "PORT $ftp(LocalAddr),$ftp(DataPort)"
			  	set ftp(State) put_open
			  } else {
				set errmsg "Error setting port!"
			  }
			}
			
		put_open {
			  switch $rc {
			    2       {
			               PutsCtrlSock "STOR $ftp(RemoteFilename)"
			               set ftp(State) put_sent
			            }
			    default {
				       set errmsg "Error setting port!"
				       set complete_with 0
				     }
			  }
			}
			
		put_sent	{
			  switch $rc {
			    1       {
			               set ftp(State) put_close
			            }
			    default {
				       set errmsg "Error opening connection!"
				       set complete_with 0
				     }
			  }
			}
			
		put_close	{
			  switch $rc {
			    2       {
			  	       set complete_with 1
			            }
			    default {
				       set errmsg "Error storing file \"$ftp(RemoteFilename)\"!"
				       set complete_with 0
				     }
			  }
			}
			
		get 	{
			  if {[OpenDataConn]} {
			    	PutsCtrlSock "PORT $ftp(LocalAddr),$ftp(DataPort)"
			  	set ftp(State) get_open
			  } else {
				set errmsg "Error setting port!"
			  }
			}
			
		reget 	{
			  if {[OpenDataConn]} {
			    	PutsCtrlSock "PORT $ftp(LocalAddr),$ftp(DataPort)"
			  	set ftp(State) restart
			  } else {
				set errmsg "Error setting port!"
			  }
			}
			
		restart {
			  switch $rc {
			    2       { if {$ftp(FileSize) != 0} {
					PutsCtrlSock "REST $ftp(FileSize)"
				      } 
	               		      set ftp(State) get_open
				    }
			    default {
				       set errmsg "Error restarting filetransfer of \"$ftp(RemoteFilename)\"!"
				       set complete_with 0
				     }
			  }
			}
			
		get_open {
			  switch $rc {
			    2       -
			    3       {
			               PutsCtrlSock "RETR $ftp(RemoteFilename)"
			               set ftp(State) get_sent
			            }
			    default {
				       set errmsg "Error setting port!"
				       set complete_with 0
				     }
			  }
			}
			
		get_sent	{
			  switch $rc {
			    1       {
			               set ftp(State) get_close
			            }
			    default {
				       set errmsg "Error retrieving file \"$ftp(RemoteFilename)\"!"
				       set complete_with 0
				     }
			  }
			}
		get_close	{
			  switch $rc {
			    2       {
			  	       set complete_with 1
			            }
			    default {
				       set errmsg "Error retrieving file \"$ftp(RemoteFilename)\"!"
				       set complete_with 0
				     }
			  }
			}

	}

	# finish waiting 
	if {[info exist complete_with]} {
		WaitComplete $complete_with
	}

	# display control channel message
	if {[info exist msg]} {
		DisplayMsg $msg control
	}
	
	# display error message
	if {[info exist errmsg]} {
		DisplayMsg $errmsg error
	}

}

#############################################################################
#
# Type --
#
# REPRESENTATION TYPE - Sets the file transfer type to ascii or binary.
# (exported)
#
# Arguments:
# type - 		specifies the representation type (ascii|binary)
# 

proc Type {type} {
variable ftp 

	if ![info exists ftp(State)] {
		DisplayMsg "Not connected!" error
		return
	}

	set ftp(Type) $type

	set ftp(State) type
	StateHandler
	
	# wait for synchronization
	WaitOrTimeout 
	

}

#############################################################################
#
# NList --
#
# NAME LIST - This command causes a directory listing to be sent from
# server to user site.
# (exported)
# 
# Arguments:
# dir - 		The $dir should specify a directory or other system 
#			specific file group descriptor; a null argument 
#			implies the current directory. 
#
# Arguments:
# dir - 		directory to list 
# 
# Returns:
# sorted list of files or {} if listing fails

proc NList {{dir ""}} {
variable ftp 

	if ![info exists ftp(State)] {
		DisplayMsg "Not connected!" error
		return
	}

	Type ascii

	set ftp(List) {}
	if { $dir == "" } {
		set ftp(Dir) ""
	} else {
		set ftp(Dir) " $dir"
	}

	set ftp(State) nlist
	StateHandler

	# wait for synchronization
	if {[WaitOrTimeout]} { 
		unset ftp(Dir)
		return [lsort $ftp(List)]
	} else {
		Abort
		return {}
	 }

}

#############################################################################
#
# List --
#
# LIST - This command causes a list to be sent from the server
# to user site.
# (exported)
# 
# Arguments:
# dir - 		If the $dir specifies a directory or other group of 
#			files, the server should transfer a list of files in 
#			the specified directory. If the $dir specifies a file
#			then the server should send current information on the
# 			file.  A null argument implies the user's current 
#			working or default directory.  
# 
# Returns:
# list of files or {} if listing fails

proc List {{dir ""}} {
variable ftp 

	if ![info exists ftp(State)] {
		DisplayMsg "Not connected!" error
		return
	}

	Type ascii

	set ftp(List) {}
	if { $dir == "" } {
		set ftp(Dir) ""
	} else {
		set ftp(Dir) " $dir"
	}

	set ftp(State) list
	StateHandler

	# wait for synchronization
	if {[WaitOrTimeout]} { 
		
		# clear "total"-line
		set l [split $ftp(List) "\n"]
		set index [lsearch -regexp $l "^total"]
		if { $index != "-1" } { 
			set l [lreplace $l $index $index]
		}
		# clear blank line
		set index [lsearch -regexp $l "^$"]
		if { $index != "-1" } { 
			set l [lreplace $l $index $index]
		}
		
		return $l
	} else {
		Abort
		return {}
	}
}

#############################################################################
#
# ModTime --
#
# MODIFICATION TIME - This command gets the last modification time of the
# file on the remote machine.
# (exported)
# 
# Arguments:
# filename - 		specifies the remote file name
# 
# Returns:
# clock -		files date and time as a system-depentend integer
#			value in seconds (see tcls clock command)
			

proc ModTime {{filename ""}} {
variable ftp 

	if ![info exists ftp(State)] {
		DisplayMsg "Not connected!" error
		return
	}
	
	if { $filename == "" } {
		return 
	} 

	set ftp(File) $filename
	set ftp(DateTime) ""
	
	set ftp(State) modtime
	StateHandler

	# wait for synchronization
	WaitOrTimeout
	
	unset ftp(File)
		
	if { [string length $ftp(DateTime)] != 14 } {
		set clock "-1"
	} else {
		scan $ftp(DateTime) "%4s%2s%2s%2s%2s%2s" year month day hour min sec
		set clock [clock scan "$month/$day/$year $hour:$min:$sec" -gmt 1]
		unset year month day hour min sec
	}

	return $clock

}

#############################################################################
#
# Pwd --
#
# PRINT WORKING DIRECTORY - Causes the name of the current working directory.
# (exported)
# 
# Arguments:
# None.
# 
# Returns:
# current directory name

proc Pwd {} {
variable ftp 

	if ![info exists ftp(State)] {
		DisplayMsg "Not connected!" error
		return
	}

	set ftp(Dir) {}

	set ftp(State) pwd
	StateHandler

	# wait for synchronization
	WaitOrTimeout
	
	return $ftp(Dir)
}

#############################################################################
#
# Cd --
#   
# CHANGE DIRECTORY - Sets the working directory on the server host.
# (exported)
# 
# Arguments:
# dir -			pathname specifying a directory

proc Cd {dir} {
variable ftp 

	if ![info exists ftp(State)] {
		DisplayMsg "Not connected!" error
		return
	}

	set ftp(Dir) $dir

	set ftp(State) cd
	StateHandler

	# wait for synchronization
	WaitOrTimeout

	unset ftp(Dir)
}

#############################################################################
#
# MkDir --
#
# MAKE DIRECTORY - This command causes the directory specified in the $dir
# to be created as a directory (if the $dir is absolute) or as a subdirectory
# of the current working directory (if the $dir is relative).
# (exported)
# 
# Arguments:
# dir -			new directory name

# The ftp mdir command
proc MkDir {dir} {
variable ftp 

	if ![info exists ftp(State)] {
		DisplayMsg "Not connected!" error
		return
	}

	set ftp(Dir) $dir

	set ftp(State) mkdir
	StateHandler

	# wait for synchronization
	WaitOrTimeout

	unset ftp(Dir)
}

#############################################################################
#
# RmDir --
#
# REMOVE DIRECTORY - This command causes the directory specified in $dir to 
# be removed as a directory (if the $dir is absolute) or as a 
# subdirectory of the current working directory (if the $dir is relative).
# (exported)
#
# Arguments:
# dir -			directory name

proc RmDir {dir} {
variable ftp 

	if ![info exists ftp(State)] {
		DisplayMsg "Not connected!" error
		return
	}

	set ftp(Dir) $dir

	set ftp(State) rmdir
	StateHandler

	# wait for synchronization
	# synchronize with procedure StateHandler
	WaitOrTimeout

	unset ftp(Dir)
}

#############################################################################
#
# Delete --
#
# DELETE - This command causes the file specified in $file to be deleted at 
# the server site.
# (exported)
# 
# Arguments:
# file -			file name

proc Delete {file} {
variable ftp 

	if ![info exists ftp(State)] {
		DisplayMsg "Not connected!" error
		return
	}

	set ftp(File) $file

	set ftp(State) delete
	StateHandler

	# wait for synchronization
	WaitOrTimeout

	unset ftp(File)
}

#############################################################################
#
# Rename --
#
# RENAME FROM TO - This command causes the file specified in $from to be 
# renamed at the server site.
# (exported)
# 
# Arguments:
# from -			specifies the old file name of the file which 
#				is to be renamed
# to -				specifies the new file name of the file 
#				specified in the $from agument

proc Rename {from to} {
variable ftp 

	if ![info exists ftp(State)] {
		DisplayMsg "Not connected!" error
		return
	}

	set ftp(RenameFrom) $from
	set ftp(RenameTo) $to

	set ftp(State) rename

	StateHandler

	# wait for synchronization
	WaitOrTimeout

	unset ftp(RenameFrom)
	unset ftp(RenameTo)
}

#############################################################################
#
# ElapsedTime --
#
# Gets the elapsed time for file transfer
# 
# Arguments:
# stop_time - 		ending time

proc ElapsedTime {stop_time} {
variable ftp

	set elapsed [expr $stop_time - $ftp(Start_Time)]
	if { $elapsed == 0 } { set elapsed 1}
	set persec [expr $ftp(Total) / $elapsed]
	DisplayMsg "$ftp(Total) bytes sent in $elapsed seconds ($persec Bytes/s)"
}

#############################################################################
#
# PUT --
#
# STORE DATA - Causes the server to accept the data transferred via the data 
# connection and to store the data as a file at the server site.  If the file
# exists at the server site, then its contents shall be replaced by the data
# being transferred.  A new file is created at the server site if the file
# does not already exist.
# (exported)
#
# Arguments:
# source -			local file name
# dest -			remote file name, if unspecified, ftp assigns
#				the local file name.
# Returns:
# 0 -			file not stored
# 1 - 			OK

proc Put {source {dest ""}} {
variable ftp 

	if ![info exists ftp(State)] {
		DisplayMsg "C: Not connected!" error
		return 0
	}

	if ![file exist $source] {
		DisplayMsg "File \"$source\" not exist" error
		return 0
     	}
			
	if { $dest == "" } {
		set dest $source
	}

	set ftp(LocalFilename) $source
	set ftp(RemoteFilename) $dest

	set ftp(SourceCI) [open $ftp(LocalFilename) r]
	if { $ftp(Type) == "ascii" } {
		fconfigure $ftp(SourceCI) -buffering line -blocking 1
	} else {
		fconfigure $ftp(SourceCI) -buffering line -translation binary -blocking 1
	}

	set ftp(State) put
	StateHandler

	# wait for synchronization
	if {[WaitOrTimeout]} {
		ElapsedTime [clock seconds]
		return 1
	} else {
		Abort
		return 0
	}

}

#############################################################################
#
# Get --
#
# RETRIEVE DATA - Causes the server to transfer a copy of the specified file
# to the local site at the other end of the data connection.
# (exported)
#
# Arguments:
# source -			remote file name
# dest -			local file name, if unspecified, ftp assigns
#				the remote file name.
# Returns:
# 0 -			file not retrieved
# 1 - 			OK

proc Get {source {dest ""}} {
variable ftp 

	if ![info exists ftp(State)] {
		DisplayMsg "Not connected!" error
		return 0
	}

	if { $dest == "" } {
		set dest $source
	}

	set ftp(RemoteFilename) $source
	set ftp(LocalFilename) $dest

	set rc [catch {set ftp(DestCI) [open $ftp(LocalFilename) w]} msg]
	if { $rc != 0 } {
		DisplayMsg "$msg" error
		return 0
	}

	if { $ftp(Type) == "ascii" } {
		fconfigure $ftp(DestCI) -buffering line -blocking 1
	} else {
		fconfigure $ftp(DestCI) -buffering line -translation binary -blocking 1
	}

	set ftp(State) get
	StateHandler

	# wait for synchronization
	if {[WaitOrTimeout]} {
		ElapsedTime [clock seconds]
		return 1
	} else {
		Abort
		return 0
	}

}

#############################################################################
#
# Reget --
#
# RESTART RETRIEVING DATA - Causes the server to transfer a copy of the specified file
# to the local site at the other end of the data connection like get but skips over 
# the file to the specified data checkpoint. 
# (exported)
#
# Arguments:
# source -			remote file name
# dest -			local file name, if unspecified, ftp assigns
#				the remote file name.
# Returns:
# 0 -			file not retrieved
# 1 - 			OK

proc Reget {source {dest ""}} {
variable ftp 

	if ![info exists ftp(State)] {
		DisplayMsg "Not connected!" error
		return 0
	}

	if { $dest == "" } {
		set dest $source
	}

	set ftp(RemoteFilename) $source
	set ftp(LocalFilename) $dest

	if [file exist $ftp(LocalFilename)] {
		set ftp(FileSize) [file size $ftp(LocalFilename)]
	} else {
		set ftp(FileSize) 0
	}
	
	set rc [catch {set ftp(DestCI) [open $ftp(LocalFilename) a]} msg]
	if { $rc != 0 } {
		DisplayMsg "$msg" error
		return 0
	}

	if { $ftp(Type) == "ascii" } {
		fconfigure $ftp(DestCI) -buffering line -blocking 1
	} else {
		fconfigure $ftp(DestCI) -buffering line -translation binary -blocking 1
	}

	set ftp(State) reget
	StateHandler

	# wait for synchronization
	if {[WaitOrTimeout]} {
		ElapsedTime [clock seconds]
		return 1
	} else {
		Abort
		return 0
	}

}

#############################################################################
#
# Newer --
#
# GET NEWER DATA - Get the file only if the modification time of the remote 
# file is more recent that the file on the current system. If the file does
# not exist on the current system, the remote file is considered newer.
# Otherwise, this command is identical to get. 
# (exported)
#
# Arguments:
# filename -			file name
#
# Returns:
# 0 -			file not retrieved
# 1 - 			OK

proc Newer {{filename ""}} {
variable ftp 

	if ![info exists ftp(State)] {
		DisplayMsg "Not connected!" error
		return 0
	}

	if { $filename == "" } {
		return 0
	} 
	
	# get remote modification time
	set rmt [ModTime $filename]
	if { $rmt == "-1" } {
		return 0
	}

	# get local modification time
	if [file exist $filename] {
		set lmt [file mtime $filename]
	} else {
		set lmt 0
	}
	
	# remote file is older than local file
	if { $rmt < $lmt } {
		return 0
	}

	# remote file is newer than local file or local file doesn't exist
	# get it
	set rc [Get $filename]
	return $rc
		
}

#############################################################################
#
# Quote -- 
#
# The arguments specified are sent, verbatim, to the remote FTP server.     
#
# Arguments:
# 	arg1 arg2 ...

proc Quote {args} {
variable ftp

	if ![info exists ftp(State)] {
		DisplayMsg "Not connected!" error
		return
	}

	set ftp(Cmd) $args

	set ftp(State) quote
	StateHandler

	# wait for synchronization
	WaitOrTimeout
	
	unset ftp(Cmd)

}


#############################################################################
#
# Abort -- 
#
# ABORT - Tells the server to abort the previous FTP service command and 
# any associated transfer of data. The control connection is not to be 
# closed by the server, but the data connection must be closed.
#
# Arguments:
# None.

proc Abort {} {
variable ftp

	if ![info exists ftp(State)] {
		DisplayMsg "Not connected!" error
		return
	}

	set ftp(State) abort
	StateHandler

	# wait for synchronization
	WaitOrTimeout

	catch {close $ftp(DestCI)}
	catch {close $ftp(SourceCI)}
	catch {close $ftp(DummySock)}
}

#############################################################################
#
# Close -- 
#
# Terminates a ftp session and if file transfer is not in progress, the server
# closes the control connection.  If file transfer is in progress, the 
# connection will remain open for result response and the server will then
# close it. 
# (exported)
# 
# Arguments:
# None.

proc Close {} {
variable ftp

	if ![info exists ftp(State)] {
		DisplayMsg "Not connected!" error
		return
	}

	set ftp(State) quit
	StateHandler

	# wait for synchronization
	WaitOrTimeout

	catch {close $ftp(CtrlSock)}
	catch {unset ftp(State)}
}

#############################################################################
#
# Open --
#
# Starts the ftp session and sets up a ftp control connection.
# (exported)
# 
# Arguments:
# server - 		The ftp server hostname.
# user -		A string identifying the user. The user identification 
#			is that which is required by the server for access to 
#			its file system.  
# passwd -		A string specifying the user's password.
# options -		-blocksize size		writes "size" bytes at once
#						(default 4096)
#			-timeout seconds	if non-zero, sets up timeout to
#						occur after specified number of
#						seconds (default 120)
# 
# Returns:
# 0 -			Not logged in
# 1 - 			User logged in

proc Open {server user passwd {args ""}} {
variable ftp
variable DEBUG 
variable VERBOSE
upvar #0 finished state

	if [info exists ftp(State)] {
       		DisplayMsg "C: Mmh, another attempt to open a new connection? There is already a hot wire!" error
		return 0
	}

	# default NO DEBUG
	if {![info exist DEBUG]} {
		set DEBUG 0
	}

	# default NO VERBOSE
	if {![info exist VERBOSE]} {
		set VERBOSE 0
	}
	
	if {$DEBUG} {
		DisplayMsg "Starting new connection with: "
	}
	
	set ftp(User) 		$user
	set ftp(Passwd) 	$passwd
	set ftp(RemoteHost) 	$server
	set ftp(LocalHost) 	[info hostname]
	set ftp(DataPort) 	0
	set ftp(Type) 		{}
	set ftp(Blocksize) 	4096	
	set ftp(Timeout) 	1000	

	set ftp(State) 		user
	
	# set state var
	set state(control) ""
	
	# Get and set possible options
	set options {-blocksize -timeout}
	foreach {option value} $args {
		if { ([lsearch -exact $options $option] != "-1") && [regexp {^[0-9]+$} $value] } {
				if {$DEBUG} {
					DisplayMsg "  $option = $value"
				}
				regexp {^-(.?)(.*)$} $option all first rest
				set option "[string toupper $first]$rest"
				set ftp($option) $value
		} 
	}
	if { $DEBUG && ($args == "") } {
		DisplayMsg "  no option"
	}
	
	# No call of StateHandler is required at this time.
	# StateHandler at first time is called automatically
	# by a fileevent for the control channel.

	# Try to open a control connection
	if ![OpenControlConn] { return 0 }

	# waits for synchronization
	#   0 ... Not logged in
	#   1 ... User logged in
	if {[WaitOrTimeout]} {
		# default mode is binary
		Type binary
		return 1
	} else {
		# close connection if not logged in
		Close
		return 1
	}
}

#############################################################################
#
# CopyNext --
#
# recursive background copy procedure for ascii/binary file I/O
# 
# Arguments:
# bytes - 		indicates how many bytes were written on $ftp(DestCI)

proc CopyNext {bytes {error {}}} {
variable ftp
variable DEBUG
upvar #0 finished state

	# summary bytes		
	incr ftp(Total) $bytes

	# setup new timeout handler
	after cancel $ftp(Wait)
	set ftp(Wait) [after [expr $ftp(Timeout) * 1000] [namespace current]::Timeout]

	if {$DEBUG} {
		DisplayMsg "-> $ftp(Total) bytes $ftp(SourceCI) -> $ftp(DestCI)" 
	}

	if {$error != ""} {
		catch {close $ftp(DestCI)}
		catch {close $ftp(SourceCI)}
   		unset state(data)
		DisplayMsg $error error

	} elseif {[eof $ftp(SourceCI)]} {
		close $ftp(DestCI)
		close $ftp(SourceCI)
   		unset state(data)
		DisplayMsg "D: Port closed" data

	} else {
		fcopy $ftp(SourceCI) $ftp(DestCI) -command [namespace current]::CopyNext -size $ftp(Blocksize)

	}

}

#############################################################################
#
# HandleList --
#
# Handles ascii/binary data transfer for Put and Get 
# 
# Arguments:
# sock - 		socket name (data channel)

proc HandleData {sock} {
variable ftp 

	# Turn off any fileevent handlers
	fileevent $sock writable {}		
	fileevent $sock readable {}
	
	# perform fcopy	
	set ftp(Total) 0
	set ftp(Start_Time) [clock seconds]
	fcopy $ftp(SourceCI) $ftp(DestCI) -command [namespace current]::CopyNext -size $ftp(Blocksize)
}

#############################################################################
#
# HandleList --
#
# Handles ascii data transfer for list commands
# 
# Arguments:
# sock - 		socket name (data channel)

proc HandleList {sock} {
variable ftp 
upvar #0 finished state

	if ![eof $sock] {
		set buffer [read $sock]
		if { $buffer != "" } {
			set ftp(List) [append ftp(List) $buffer]
		}	
	} else {
   		close $sock
   		unset state(data)
		DisplayMsg "D: Port closed" data
	} 
}

#############################################################################
#
# InitDataConn --
#
# Configures new data channel for connection to ftp server 
# ATTENTION! The new data channel "sock" is not the same as the 
# server channel, it's a dummy.
# 
# Arguments:
# sock -		the name of the new channel
# addr -		the address, in network address notation, 
#			of the client's host,
# port -		the client's port number

proc InitDataConn {sock addr port} {
variable ftp
upvar #0 finished state

	# If the new channel is accepted, the dummy channel will closed
	close $ftp(DummySock)
	unset ftp(DummySock)

	set state(data) 0

	# Configure translation mode
	if { $ftp(Type) == "ascii" } {
		fconfigure $sock -buffering line -blocking 1
	} else {
		fconfigure $sock -buffering line -translation binary -blocking 1
	}

	# assign fileevent handlers, source and destination CI (Channel Identifier)
	switch -regexp $ftp(State) {

		^list {
			  fileevent $sock readable [list [namespace current]::HandleList $sock]
			  set ftp(SourceCI) $sock		  
			}

		^get	{
			  fileevent $sock readable [list [namespace current]::HandleData $sock]
			  set ftp(SourceCI) $sock			  
			}

		^put {
			  fileevent $sock writable [list [namespace current]::HandleData $sock]
			  set ftp(DestCI) $sock			  
			}
	}

	DisplayMsg "D: Connection from $addr:$port" data
}

#############################################################################
#
# OpenDataConn --
#
# Opens a ftp data connection
# 
# Arguments:
# None.
# 
# Returns:
# 0 -			no connection
# 1 - 			connection established

proc OpenDataConn {} {
variable ftp

	# Port address 0 is a dummy used to give the server the responsibility 
	# of getting free new port addresses for every data transfer.
	set rc [catch {set ftp(DummySock) [socket -server InitDataConn 0]} msg]
	if { $rc != 0 } {
       		DisplayMsg "$msg" error
       		return 0
	}

	# get a new local port address for data transfer and convert it to a format
	# which is useable by the PORT command
	set p [lindex [fconfigure $ftp(DummySock) -sockname] 2]
	DisplayMsg "D: Port is $p" data
	set ftp(DataPort) "[expr "$p / 256"],[expr "$p % 256"]"

	return 1
}

#############################################################################
#
# OpenControlConn --
#
# Opens a ftp control connection
# 
# Arguments:
# None.
# 
# Returns:
# 0 -			no connection
# 1 - 			connection established

proc OpenControlConn {} {
variable ftp
variable DEBUG

	# open a control channel
        set rc [catch {set ftp(CtrlSock) [socket $ftp(RemoteHost) 21]} msg]
	if { $rc != 0 } {
       		DisplayMsg "C: No connection to server!" error
		if {$DEBUG} {
			DisplayMsg "[list $msg]" error
		}
       		unset ftp(State)
       		return 0
	}
	# configure control channel
	fconfigure $ftp(CtrlSock) -buffering line -blocking 1 -translation {auto crlf}
        fileevent $ftp(CtrlSock) readable [list [namespace current]::StateHandler $ftp(CtrlSock)]
	
	# prepare local ip address for PORT command (convert pointed format to comma format)
	set ftp(LocalAddr) [lindex [fconfigure $ftp(CtrlSock) -sockname] 0]
	regsub -all "\[.\]" $ftp(LocalAddr) "," ftp(LocalAddr) 

	# report ready message
	set peer [fconfigure $ftp(CtrlSock) -peername]
	DisplayMsg "C: Connection from [lindex $peer 0]:[lindex $peer 2]" control
	
	return 1
}


# not forgotten close-brace (end of namespace)
}










