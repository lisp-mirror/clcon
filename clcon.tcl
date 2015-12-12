#!/bin/sh
# -*- tcl -*-
# \
exec wish "$0" ${1+"$@"}
#
## clcon.tcl
## 
## Common Lisp IDE
## 
## A fork of tkcon.tcl
##
## Originally based off Brent Welch's Tcl Shell Widget
## (from "Practical Programming in Tcl and Tk")
##
## Thanks to the following (among many) for early bug reports & code ideas:
## Steven Wahl, Jan Nijtmans, Mark Crimmins, Wart
##
## Copyright (c) 1995-2011 Jeffrey Hobbs, jeff(a)hobbs(.)org
## Initiated: Thu Aug 17 15:36:47 PDT 1995
##
## source standard_disclaimer.tcl
## source bourbon_ware.tcl
## 
## Copyright (c) Denis Budyak 2015
## 

encoding system utf-8

# Try to load record_definition as early as possible to get access to all
# source locations
namespace eval ::tkcon {
    variable ScriptDirectory 
    set ScriptDirectory [file dirname [info script]]
}


proc ::tkcon::CheckPatchlevel {} {
    regsub -all {[a-zA-Z.]} [info patchlevel] {.} dd
    set p [split $dd {.}]
    set ma [lindex $p 0]
    set mi [lindex $p 1]
    set su [lindex $p 2]
    if { $ma < 8 || $mi < 6 || $su < 2 } {
        error "Clcon requires Tcl/Tk >= 8.6.2"
    }
}


 ::tkcon::CheckPatchlevel



## TkconSourceHere - buddens command to load file from the same dir where
# clcon itself is located. Be sure to load into main interpreter when you need it:
#
proc TkconSourceHere { filename } {
    variable ::tkcon::ScriptDirectory
    source [file join $::tkcon::ScriptDirectory $filename]
}

TkconSourceHere record_definition.tcl
TkconSourceHere named_args.tcl

package require Tk 8.4

option add *tearOff 0

# package require tablelist - it must work after toplevel window creation


## This is a way to add directory for wcb package
lappend ::auto_path [file join [file dirname [info script]] "lib/wcb3.5"]
package require wcb

#source /s2/cl-tk/init-cl-tk-tcl-runtime.tcl


# We need to load some package to get what's available, and we
# choose ctext because we'll use it if its available in the editor
# catch {package require ctext}
foreach pkg [info loaded {}] {
    set file [lindex $pkg 0]
    set name [lindex $pkg 1]
    if {![catch {set version [package require $name]}]} {
	if {[package ifneeded $name $version] eq ""} {
	    package ifneeded $name $version [list load $file $name]
	}
    }
}

# Unset temporary global vars
catch {unset pkg file name version}

namespace eval ::clconcmd {
    # Namespace for clcon commands
    # ??? Do we need to alias it to slave interpreters??? I think yes as
    # we need to run them in context of one console
}


# Initialize the ::tkcon namespace
#
namespace eval ::tkcon {
    # when modifying this line, make sure that the auto-upgrade check
    # for version still works.
    variable VERSION "0.3.3"
    # The OPT variable is an array containing most of the optional
    # info to configure.  COLOR has the color data.
    variable OPT
    variable COLOR

    # PRIV is used for internal data that only tkcon should fiddle with.
    variable PRIV
    set PRIV(WWW) [info exists embed_args]
    set PRIV(AQUA) [expr {$::tcl_version >= 8.4 && [tk windowingsystem] == "aqua"}]
    set PRIV(CTRL) [expr {$PRIV(AQUA) ? "Command-" : "Control-"}]
    set PRIV(ACC) [expr {$PRIV(AQUA) ? "Command-" : "Ctrl+"}]

    variable EXPECT 0
    # 1 - enable unknown from tkcon, 0 - disable. Processed at startup only (I guess)
    # -1 - enable unknown completely in main interpreter and all slaves
    variable ENABLE_UNKNOWN -1
    

    # Continuations for sync events - hope we have no sync dialog yet - all sync dialogs
    # should be of the form:
    # sync call -> sync reply or
    # sync call -> eof on lisp side
    # so we don't need the variable yet
    # but we need to distinguish before sync and async events, so we need to store at least one
    # async continuation. contains dictionary: id->{procname context}.
    # when :return or :abort event is received, procname is invoked on context, event, id
    variable SWANKSyncContinuation {}

    # Continuations for async events
    # dictionary: id->{procname context} for async events only
    variable SWANKAsyncContinuations
    # Queue of all events (list, first element of a list is a first element of a queue)"
    variable SWANKEventQueue {}

    # 1 when we are processing synchronous negotiation
    variable SWANKIsInSyncMode 0
}

## putd - output debugging string
## like tkcon_puts, but paints at some other color
# ARGS:	string
# Outputs:	the string with a color-coded text tag
## 
proc putd {arg1} {
    variable ::tkcon::OPT

    if { ![info exists ::tkcon::OPT] } {
        error "it is too early to use putd. Try 'puts stderr'"
    }
    if { $::tkcon::OPT(putd-enabled) } {
        global putdChannel
        global putdChannelOpen 
        if {![info exists putdChannelOpen]} {
            set outputFile $OPT(putd-output-file)
            if {$outputFile eq {}} {
                error "putd-output-file option is missing"
            }
            set putdChannel [open $outputFile w+]
            set putdChannelOpen 1
        }
        puts $putdChannel $arg1
        flush $putdChannel
    }
}


proc ::tkcon::ReloadSomeIDESources1 {} {
    # also we need record_definition.tcl, but we never reload it
    # as any error in it likely means a spoiled image.
    TkconSourceHere util.tcl
    TkconSourceHere gui_util.tcl
    TkconSourceHere ide_structure.tcl
    TkconSourceHere ro_outer_synonyms.ro_out.tcl
    TkconSourceHere text2odu.tcl
    TkconSourceHere btext.tcl
    TkconSourceHere cons.tcl
    TkconSourceHere clcon_key.tcl
    TkconSourceHere clcon_text.tcl
    TkconSourceHere swank-connection.swcnn.tcl
    TkconSourceHere swank-io.tcl
    TkconSourceHere swank-applications.tkcon.tcl
    TkconSourceHere console-cmd.tcl
    TkconSourceHere slimv-clcon.tcl
    TkconSourceHere findreplace.tcl
    TkconSourceHere findbox.fndrpl.tcl
    TkconSourceHere recent.tcl
    TkconSourceHere window_menu.tcl
}    


::tkcon::ReloadSomeIDESources1

## ::tkcon::Init - inits tkcon
#
# Calls:	::tkcon::InitUI
# Outputs:	errors found in tkcon's resource file
# Called from AtSource at least, buf also from tkcon command. 
##
proc ::tkcon::Init {args} {
    variable VERSION
    variable OPT
    variable COLOR
    variable PRIV
    variable ENABLE_UNKNOWN
    global tcl_platform env tcl_interactive errorInfo

    set tcl_interactive 1
    set argc [llength $args]

    ##
    ## When setting up all the default values, we always check for
    ## prior existence.  This allows users who embed tkcon to modify
    ## the initial state before tkcon initializes itself.
    ##

    # bg == {} will get bg color from the main toplevel (in InitUI)
    foreach {key default} {
	bg		{}
	blink		\#FFFF00
	cursor		\#000000
	disabled	\#4D4D4D
	proc		\#008800
	var		\#FFC0D0
	prompt		\#8F4433
	stdin		\#000000
	stdout		\#0000FF
	stderr		\#FF0000
        debug_string    \#888888
        ldbg_frame_bg   \#F8F8D0
        error_browser_serious_bg \#F8F8D0
        error_browser_fatal_bg   \#F8D0D0
    } {
	if {![info exists COLOR($key)]} { set COLOR($key) $default }
    }

    if {![info exists OPT(font)]} {
        if {$tcl_platform(platform) == "windows"} {
            set OPT(font) {{Lucida Console} 14}
        } 
    }
    
    # tclexpandorder could also include 'Methodname' for XOTcl/NSF methods
    foreach {key default} {
	autoload	{}
	blinktime	500
	blinkrange	1
	buffer		1512

        # does not work with SWANK
	maxlinelen	0
        
	cols		80
	debugPrompt	{(level \#$level) debug [history nextid] > }
	dead		{}
	edit		::edt::edit
	tclexpandorder	{Variable Procname}
	font		{}
	history		500
	hoterrors	1
	library		{}
	lightbrace	1
	lightcmd	1
	maineval	{}
	maxmenu		18
	nontcl		0
	prompt1		{ignore this, it's set below}
	rows		20
	scrollypos	right
	showmultiple	1
	showstatusbar	1
	slaveeval	{}
	slaveexit	close
	subhistory	1
	tabspace	8
	gc-delay	60000
	gets		{congets}
	overrideexit	1
	usehistory	1
        putd-output-file {}
        putd-enabled    0
        oduvan-backend  1
        swank-ip        127.0.0.1
        swank-port      4009

        # does not work with SWANK
	resultfilter	{}

        # there is also 'exec' option equal to 'slave', but now we work
        # in main interpreter and it is specified ~70 loc below
    } {
	if {![info exists OPT($key)]} { set OPT($key) $default }
    }

    foreach {key default} {
	app		{}
	appname		{}
	apptype		slave
	namesp		::
	cmd		{}
	cmdbuf		{}
	cmdsave		{}
	event		1
	deadapp		0
	deadsock	0
	debugging	0
	displayWin	.
	histid		0
	find		{}
	find,case	0
	find,reg	0
	errorInfo	{}
	showOnStartup	1
	slaveprocs	{
	    alias clear dir dump echo idebug lremove
	    tkcon_puts tkcon_gets observe observe_var unalias which what
	}

	docs		"https://bitbucket.org/budden/clcon"
	email		{}
	root		.
	uid		0
	tabs		{}

        SwankThread      {}
        SwankReplReady   0  
        CurrentPackageName "COMMON-LISP-USER"
        CurrentPackageDisplayName "CL-USER"
    } {
	if {![info exists PRIV($key)]} { set PRIV($key) $default }
    }
    foreach {key default} {
	slavealias	{ $OPT(edit) more less tkcon }
    } {
	if {![info exists PRIV($key)]} { set PRIV($key) [subst $default] }
    }
    set PRIV(version) $VERSION

    if {[info exists PRIV(name)]} {
	set title $PRIV(name)
    } else {
	MainInit
	# some main initialization occurs later in this proc,
	# to go after the UI init
	set MainInit 1
	set title Main
    }

    ## NOTES FOR STAYING IN PRIMARY INTERPRETER:
    ##
    ## If you set ::tkcon::OPT(exec) to {}, then instead of a multiple
    ## interp model, you get tkcon operating in the main interp by default.
    ## This can be useful when attaching to programs that like to operate
    ## in the main interpter (for example, based on special wish'es).
    ## You can set this from the command line with -exec ""
    ## A side effect is that all tkcon command line args will be used
    ## by the first console only.
    set OPT(exec) {}

    ## To work in slave mode (tkcon default) run
    ## clcon with arg -exec slave
    ## But this will break SWANK client. Bdden

    if {$PRIV(WWW)} {
	lappend PRIV(slavealias) history
	set OPT(prompt1) {[history nextid] % }
    } else {
        if { $ENABLE_UNKNOWN != 0 } {
            lappend PRIV(slaveprocs) tcl_unknown unknown
        }
	set OPT(prompt1) {([file tail [pwd]]) [history nextid] % }
    }

    ## If we are using the default '.' toplevel, and there appear to be
    ## children of '.', then make sure we use a disassociated toplevel.
    if {$PRIV(root) == "." && [llength [winfo children .]]} {
	set PRIV(root) .tkcon
    }

    ## Before processing user profile, create clconcmd namespace so that use can create his commands
    namespace eval ::clconcmd {}

    ## Do platform specific configuration here, other than defaults
    ### Use tkcon.cfg filename for resource filename on non-unix systems
    ### Determine what directory the resource file should be in
    switch $tcl_platform(platform) {
	macintosh	{
	    if {![interp issafe]} {cd [file dirname [info script]]}
	    set envHome		PREF_FOLDER
	    set rcfile		clcon.cfg
	    set histfile	clcon.hst
	    catch {console hide}
	}
	windows		{
	    set envHome		HOME
	    set rcfile		clcon.cfg
	    set histfile	clcon.hst
	}
	unix		{
	    set envHome		HOME
	    set rcfile		.clconrc
	    set histfile	.clcon_history
	}
    }
    if {[info exists env($envHome)]} {
	set home $env($envHome)
	if {[file pathtype $home] == "volumerelative"} {
	    # Convert 'C:' to 'C:/' if necessary, innocuous otherwise
	    append home /
	}
	if {![info exists PRIV(rcfile)]} {
	    set PRIV(rcfile)	[file join $home $rcfile]
	}
	if {![info exists PRIV(histfile)]} {
	    set PRIV(histfile)	[file join $home $histfile]
	}
    }

    ## Handle command line arguments before sourcing resource file to
    ## find if resource file is being specified (let other args pass).
    if {[set i [lsearch -exact $args -rcfile]] != -1} {
	set PRIV(rcfile) [lindex $args [incr i]]
    }

    if {!$PRIV(WWW) && [file exists $PRIV(rcfile)]} {
	set code [catch {uplevel \#0 [list source $PRIV(rcfile)]} err]
    }

    if {[info exists env(TK_CON_LIBRARY)]} {
	lappend ::auto_path $env(TK_CON_LIBRARY)
    } elseif {$OPT(library) != ""} {
	lappend ::auto_path $OPT(library)
    }

    if {![info exists ::tcl_pkgPath]} {
	set dir [file join [file dirname [info nameofexec]] lib]
	if {[llength [info commands @scope]]} {
	    set dir [file join $dir itcl]
	}
	catch {source [file join $dir pkgIndex.tcl]}
    }
    catch {tclPkgUnknown dummy-name dummy-version}

    ## Handle rest of command line arguments after sourcing resource file
    ## and slave is created, but before initializing UI or setting packages.
    set slaveargs {}
    set slavefiles {}
    set slaveargv0 {}
    set truth {^(1|yes|true|on)$}
    for {set i 0} {$i < $argc} {incr i} {
	set arg [lindex $args $i]
	if {[string match {-*} $arg]} {
	    set val [lindex $args [incr i]]
	    ## Handle arg based options
	    switch -glob -- $arg {
		-- - -argv - -args {
		    set slaveargs [concat $slaveargs [lrange $args $i end]]
		    set ::argv $slaveargs
		    set ::argc [llength $::argv]
		    break
		}
		-color-*	{ set COLOR([string range $arg 7 end]) $val }
		-exec		{ set OPT(exec) $val }
		-main - -e - -eval	{ append OPT(maineval) \n$val\n }
		-package - -load	{
		    lappend OPT(autoload) $val
		    if {$val eq "nsf" || $val eq "nx" || $val eq "XOTcl" } {
			# If xotcl is loaded, prepend expand order for it
			set OPT(tclexpandorder) [concat Methodname $OPT(expandorder)]
		    }
		}
		-slave		{ append OPT(slaveeval) \n$val\n }
		-nontcl		{ set OPT(nontcl) [regexp -nocase $truth $val]}
		-root		{ set PRIV(root) $val }
		-font		{ set OPT(font) $val }
                -putd-output-file { set OPT(putd-output-file) $val }
                -putd-enabled   { set OPT(putd-enabled) $val }
                -oduvan-backend { set OPT(oduvan-backend) $val }
                -swank-ip       { set OPT(swank-ip) $val }
                -swank-port     { set OPT(swank-port) $val }
		-rcfile	{}
		default	{ lappend slaveargs $arg; incr i -1 }
	    }
	} elseif {[file isfile $arg]} {
	    if {$i == 0} {
		set slaveargv0 $arg
	    }
	    lappend slavefiles $arg
	} else {
	    lappend slaveargs $arg
	}
    }
    # Check params compatibility
    if {$OPT(putd-enabled) && ($OPT(putd-output-file) eq {})} {
        error "You may only set -putd-enabled to 1 when you specify -putd-output-file"
    }

    ## Create slave executable
    if {"" != $OPT(exec)} {
	InitSlave $OPT(exec) $slaveargs $slaveargv0
    } else {
	set argc [llength $slaveargs]
	set args $slaveargs
	uplevel \#0 $slaveargs
    }

    # Try not to make tkcon override too many standard defaults, and only
    # do it for the tkcon bits
    set optclass [tk appname]$PRIV(root)
    option add $optclass*Menu.tearOff 0
    option add $optclass*Menu.borderWidth 1
    option add $optclass*Menu.activeBorderWidth 1
    if {!$PRIV(AQUA)} {
	option add $optclass*Scrollbar.borderWidth 1
    }

    ## Attach to the slave, EvalAttached will then be effective
    Attach $PRIV(appname) $PRIV(apptype)
    InitUI $title
    if {"" != $OPT(exec)} {
	# override exit to DeleteTab now that tab has been created
	$OPT(exec) alias exit ::tkcon::DeleteTab $PRIV(curtab) $OPT(exec)
    }

    ## swap puts and gets with the tkcon versions to make sure all
    ## input and output is handled by tkcon
    if {![catch {rename ::puts ::tkcon_tcl_puts}]} {
	interp alias {} ::puts {} ::tkcon_puts
	if {[llength [info commands ::tcl::chan::puts]]} {
	    interp alias {} ::tcl::chan::puts {} ::tkcon_puts
	}
    }
    if {($OPT(gets) != "") && ![catch {rename ::gets ::tkcon_tcl_gets}]} {
	interp alias {} ::gets {} ::tkcon_gets
	if {[llength [info commands ::tcl::chan::gets]]} {
	    interp alias {} ::tcl::chan::gets {} ::tkcon_gets
	}
    }

    EvalSlave history keep $OPT(history)
    if {[info exists MainInit]} {
	# Source history file only for the main console, as all slave
	# consoles will adopt from the main's history, but still
	# keep separate histories
	if {!$PRIV(WWW) && $OPT(usehistory) && [file exists $PRIV(histfile)]} {
	    puts -nonewline "loading history and recent menu file ... "
	    # The history file is built to be loaded in and
	    # understood by tkcon
	    if {[catch {uplevel \#0 [list source $PRIV(histfile)]} herr]} {
		puts stderr "error:\n$herr"
		append PRIV(errorInfo) $errorInfo\n
	    }
	    set PRIV(event) [EvalSlave history nextid]
	    puts "[expr {$PRIV(event)-1}] events added"

	}
    }

    ## Autoload specified packages in slave
    set pkgs [EvalSlave package names]
    foreach pkg $OPT(autoload) {
	puts -nonewline "autoloading package \"$pkg\" ... "
	if {[lsearch -exact $pkgs $pkg]>-1} {
	    if {[catch {EvalSlave package require [list $pkg]} pkgerr]} {
		puts stderr "error:\n$pkgerr"
		append PRIV(errorInfo) $errorInfo\n
	    } else { puts "OK" }
	} else {
	    puts stderr "error: package does not exist"
	}
    }

    ## Evaluate maineval in slave
    if {($OPT(maineval) ne "") && [catch {uplevel \#0 $OPT(maineval)} merr]} {
	puts stderr "error in eval:\n$merr"
	append PRIV(errorInfo) $errorInfo\n
    }

    ## Source extra command line argument files into slave executable
    foreach fn $slavefiles {
	puts -nonewline "slave sourcing \"$fn\" ... "
	if {[catch {EvalSlave uplevel \#0 [list source $fn]} fnerr]} {
	    puts stderr "error:\n$fnerr"
	    append PRIV(errorInfo) $errorInfo\n
	} else { puts "OK" }
    }

    ## Evaluate slaveeval in slave
    if {($OPT(slaveeval) ne "")
	&& [catch {interp eval $OPT(exec) $OPT(slaveeval)} serr]} {
	puts stderr "error in slave eval:\n$serr"
	append PRIV(errorInfo) $errorInfo\n
    }
    ## Output any error/output that may have been returned from rcfile
    if {[info exists code] && $code && ($err ne "")} {
	puts stderr "error in $PRIV(rcfile):\n$err"
	append PRIV(errorInfo) $errorInfo
    }

    puts "buffer line limit:\
	[expr {$OPT(buffer)?$OPT(buffer):{unlimited}}]  \
	max line length:\
	[expr {$OPT(maxlinelen)?$OPT(maxlinelen):{unlimited}}]"

    Prompt "$title console display active (Tcl$::tcl_patchLevel / Tk$::tk_patchLevel)\n"
}

## ::tkcon::InitSlave - inits the slave by placing key procs and aliases in it
## It's arg[cv] are based on passed in options, while argv0 is the same as
## the master.  tcl_interactive is the same as the master as well.
# ARGS:	slave	- name of slave to init.  If it does not exist, it is created.
#	args	- args to pass to a slave as argv/argc
##
proc ::tkcon::InitSlave {slave {slaveargs {}} {slaveargv0 {}}} {
    variable OPT
    variable COLOR
    variable PRIV
    global argv0 tcl_interactive tcl_library env auto_path tk_library

    if {$slave eq ""} {
	return -code error "Don't init the master interpreter, goofball"
    }
    if {![interp exists $slave]} { interp create $slave }

    #interp eval $slave "proc ::CurIntPath {puts} { set result $slave \n if { \$puts ne {} } {             puts  \"CurIntPath = \$result\ (\$puts)\" } \n return \$result }"
    interp eval $slave "proc ::CurIntPath {puts} {}"
    interp eval $slave "::CurIntPath {::tkcon::InitSlave-slave}"
   
    if {[interp eval $slave info command source] == ""} {
	$slave alias source SafeSource $slave
	$slave alias load SafeLoad $slave
	$slave alias open SafeOpen $slave
	$slave alias file file
	interp eval $slave \
	    [list set auto_path [lremove $auto_path $tk_library]]
	interp eval $slave [dump var -nocomplain tcl_library env]
	interp eval $slave { catch {source [file join $tcl_library init.tcl]} }
	interp eval $slave { catch unknown }
    }

    #set cip [interp eval $slave {::CurIntPath {}}]
    #puts "$cip (aa)"
    
    # This will likely be overridden to call DeleteTab where possible
    $slave alias exit exit
    interp eval $slave {
	# Do package require before changing around puts/gets
	catch {set __tkcon_error ""; set __tkcon_error $errorInfo}
	catch {package require bogus-package-name}
	catch {rename ::puts ::tkcon_tcl_puts}
	set errorInfo ${__tkcon_error}
	unset __tkcon_error
    }
    # $slave eval { namespace eval ::clconcmd {} }
    foreach cmd $PRIV(slaveprocs) { $slave eval [dump proc $cmd] }
    foreach cmd $PRIV(slavealias) { $slave alias $cmd $cmd }
    interp alias $slave ::ls $slave ::dir -full
    interp alias $slave ::puts $slave ::tkcon_puts
    if {[llength [info commands ::tcl::chan::puts]]} {
	interp alias $slave ::tcl::chan::puts $slave ::tkcon_puts
    }
    if {$OPT(gets) != ""} {
	interp eval $slave { catch {rename ::gets ::tkcon_tcl_gets} }
	interp alias $slave ::gets $slave ::tkcon_gets
	if {[llength [info commands ::tcl::chan::gets]]} {
	    interp alias $slave ::tcl::chan::gets $slave ::tkcon_gets
	}
    }
    if {$slaveargv0 != ""} {
	# If tkcon was invoked with 1 or more filenames, then make the
	# first filename argv0 in the slave, as tclsh/wish would do it.
	interp eval $slave [list set argv0 $slaveargv0]
    } else {
	if {[info exists argv0]} {interp eval $slave [list set argv0 $argv0]}
    }
    interp eval $slave set tcl_interactive $tcl_interactive \; \
	    set auto_path [list [lremove $auto_path $tk_library]] \; \
	    set argc [llength $slaveargs] \; \
	    set argv  [list $slaveargs] \; {
	if {![llength [info command bgerror]]} {
	    proc bgerror err {
		global errorInfo
		set body [info body bgerror]
		rename ::bgerror {}
		if {[auto_load bgerror]} { return [bgerror $err] }
		proc bgerror err $body
		tkcon bgerror $err $errorInfo
	    }
	}
    }

    foreach pkg [lremove [package names] Tcl] {
	foreach v [package versions $pkg] {
	    interp eval $slave [list package ifneeded $pkg $v \
		    [package ifneeded $pkg $v]]
	}
    }
}

## ::tkcon::InitInterp - inits an interpreter by placing key
## procs and aliases in it.
# ARGS: name	- interp name
#	type	- interp type (slave|interp)
##
proc ::tkcon::InitInterp {name type} {
    variable OPT
    variable PRIV

    ## Don't allow messing up a local master interpreter
    if {($type eq "namespace")
	|| (($type eq "slave") &&
	    [regexp {^([Mm]ain|Slave[0-9]+)$} $name])} { return }
    set old [Attach]
    set oldname $PRIV(namesp)
    catch {
	Attach $name $type
	EvalAttached { catch {rename ::puts ::tkcon_tcl_puts} }
	foreach cmd $PRIV(slaveprocs) { EvalAttached [dump proc $cmd] }
	switch -exact $type {
	    slave {
		foreach cmd $PRIV(slavealias) {
		    Main [list interp alias $name ::$cmd $PRIV(name) ::$cmd]
		}
	    }
	    interp {
		set thistkcon [::send::appname]
		foreach cmd $PRIV(slavealias) {
		    EvalAttached "proc $cmd args { ::send::send [list $thistkcon] $cmd \$args }"
		}
	    }
	}
	## Catch in case it's a 7.4 (no 'interp alias') interp
	EvalAttached {
	    catch {interp alias {} ::ls {} ::dir -full}
	    if {[catch {interp alias {} ::puts {} ::tkcon_puts}]} {
		catch {rename ::tkcon_puts ::puts}
	    } elseif {[llength [info commands ::tcl::chan::puts]]} {
		catch {interp alias {} ::tcl::chan::puts {} ::tkcon_puts}
	    }
	}
	if {$OPT(gets) != ""} {
	    EvalAttached {
		catch {rename ::gets ::tkcon_tcl_gets}
		if {[catch {interp alias {} ::gets {} ::tkcon_gets}]} {
		    catch {rename ::tkcon_gets ::gets}
		} elseif {[llength [info commands ::tcl::chan::gets]]} {
		    catch {interp alias {} ::tcl::chan::gets {} ::tkcon_gets}
		}
	    }
	}
	return
    } {err}
    eval Attach $old
    AttachNamespace $oldname
    if {$err ne ""} { return -code error $err }
}

## ::tkcon::InitUI - inits UI portion (console) of tkcon
## Creates all elements of the console window and sets up the text tags
# ARGS:	root	- widget pathname of the tkcon console root
#	title	- title for the console root and main (.) windows
# Calls:	::tkcon::InitMenus, ::tkcon::Prompt
##
proc ::tkcon::InitUI {title} {
    variable OPT
    variable PRIV
    variable COLOR

    set root $PRIV(root)
    if {$root eq "."} { set w {} } else { set w [toplevel $root] }
    if {!$PRIV(WWW)} {
	wm withdraw $root
	wm protocol $root WM_DELETE_WINDOW ::tkcon::Destroy
    }
    set PRIV(base) $w

    catch {font create tkconfixed -family Courier -size -20}
    catch {font create tkconfixedbold -family Courier -size -20 -weight bold}

    set PRIV(statusbar) [set sbar [frame $w.fstatus]]
    set PRIV(tabframe)  [frame $sbar.tabs]
    set PRIV(X) [button $sbar.deltab -text "X" -command ::tkcon::DeleteTab \
		     -activeforeground red -fg red -font tkconfixedbold \
		     -highlightthickness 0 -padx 2 -pady 0 -borderwidth 1 \
		     -state disabled -relief flat -takefocus 0]
    catch {$PRIV(X) configure -overrelief raised}
    label $sbar.cursor -relief sunken -borderwidth 1 -anchor e -width 6 \
	    -textvariable ::tkcon::PRIV(StatusCursor)
    set padx [expr {![info exists ::tcl_platform(os)]
		    || ($::tcl_platform(os) ne "Windows CE")}]
    grid $PRIV(X) $PRIV(tabframe) $sbar.cursor -sticky news -padx $padx
    grid configure $PRIV(tabframe) -sticky nsw
    grid configure $PRIV(X) -pady 0 -padx 0
    grid columnconfigure $sbar 1 -weight 1
    grid rowconfigure $sbar 0 -weight 1
    grid rowconfigure $PRIV(tabframe) 0 -weight 1
    if {$PRIV(AQUA)} {
	# resize control space and correct "X" button space
	grid columnconfigure $sbar [lindex [grid size $sbar] 0] -minsize 16
	$PRIV(X) configure -pady 5 -padx 4
    }

    ## Create console tab
    set con [InitTab $w]
    set PRIV(curtab) $con

    # Only apply this for the first console
    $con configure -setgrid 1 -width $OPT(cols) -height $OPT(rows)
    bind $PRIV(root) <Configure> {
	if {"%W" == $::tkcon::PRIV(root)} {
	    scan [wm geometry [winfo toplevel %W]] "%%dx%%d" \
		::tkcon::OPT(cols) ::tkcon::OPT(rows)
	    if {[info exists ::tkcon::EXP(spawn_id)]} {
		catch {stty rows $::tkcon::OPT(rows) columns \
			   $::tkcon::OPT(cols) < $::tkcon::EXP(slave,name)}
	    }
	}
    }

    # scrollbar
    set sy [scrollbar $w.sy -takefocus 0 -command [list $con yview]]
    if {!$PRIV(WWW) && ($::tcl_platform(os) eq "Windows CE")} {
	$w.sy configure -width 10
    }

    $con configure -yscrollcommand [list $sy set]
    set PRIV(console) $con
    set PRIV(scrolly) $sy

    ## Menus
    ## catch against use in plugin
    if {[catch {menu $w.mbar} PRIV(menubar)]} {
	set PRIV(menubar) [frame $w.mbar -relief raised -borderwidth 1]
    }

    InitMenus $PRIV(menubar) $title
    Bindings

    $root configure -menu $PRIV(menubar)

    grid $con  -row 1 -column 1 -sticky news
    grid $sy   -row 1 -column [expr {$OPT(scrollypos)=="left"?0:2}] -sticky ns
    grid $sbar -row 2 -column 0 -columnspan 3 -sticky ew

    grid columnconfigure $root 1 -weight 1
    grid rowconfigure    $root 1 -weight 1

    if {!$OPT(showstatusbar)} {
	grid remove $sbar
    }

    # If we can locate the XDG icon file then make use of it.
    if {[package vsatisfies [package provide Tk] 8.6]} {
        if {[tk windowingsystem] eq "x11"} {
            if {[set icon [locate_xdg_icon tkcon-icon.png]] ne ""} {
                image create photo tkcon_icon -file $icon
                wm iconphoto $root tkcon_icon
            }
        }
    }

    if {!$PRIV(WWW)} {
	wm title $root "clcon $PRIV(version) $title"
	if {$PRIV(showOnStartup)} {
	    # this may throw an error if toplevel is embedded
	    catch {wm deiconify $root}
	}
    }
    if {$PRIV(showOnStartup)} { focus -force $PRIV(console) }
    if {$OPT(gc-delay)} {
	after $OPT(gc-delay) ::tkcon::GarbageCollect
    }
}

# Hunt around the XDG defined directories for the icon.
# Note: hicolor is the standard theme used by xdg-icon-resource.
proc ::tkcon::locate_xdg_icon {name} {
    global env
    set dirs [list /usr/local/share /usr/share]
    if {[info exists env(XDG_DATA_DIRS)]} {
        set dirs [split $env(XDG_DATA_DIRS) :]
    }
    if {[file isdirectory ~/.local/share]} {
        set dirs [linsert $dirs 0 ~/.local/share]
    }
    foreach dir $dirs {
        foreach path [list icons icons/hicolor/48x48/apps] {
            set path [file join $dir $path $name]
            if {[file exists $path]} {
                return $path
            }
        }
    }
    return ""
}

proc ::tkcon::InitTab {w} {
    variable OPT
    variable PRIV
    variable COLOR
    variable ATTACH

    # text console
    set con $w.tab[incr PRIV(uid)]

    # budden. I'm not quite sure...
    # wm protocol $con WM_DELETE_WINDOW [list wm destroy $PRIV(root)]

    # ctext -linemap 0 , clcon_text
    text $con -wrap char -foreground $COLOR(stdin) \
	-insertbackground $COLOR(cursor) -borderwidth 1 -highlightthickness 0

    $con configure -inactiveselectbackground [$con cget -selectbackground]    
    
    $con mark set output 1.0
    $con mark set limit 1.0
    if {$COLOR(bg) ne ""} {
	$con configure -background $COLOR(bg)
    }
    set COLOR(bg) [$con cget -background]
    if {$OPT(font) ne ""} {
	## Set user-requested font, if any
	$con configure -font $OPT(font)
    } elseif {$::tcl_platform(platform) ne "unix"} {
	## otherwise make sure the font is monospace
	set font [$con cget -font]
	if {![font metrics $font -fixed]} {
	    $con configure -font tkconfixed
	}
    } else {
	$con configure -font tkconfixed
    }
    set OPT(font) [$con cget -font]
    bindtags $con [list TkConsoleTextOverrides $con TkConsole TkConsolePost $PRIV(root) all]

    # scrollbar
    if {!$PRIV(WWW)} {
	if {$::tcl_platform(os) eq "Windows CE"} {
	    font configure tkconfixed -family Tahoma -size 8
	    $con configure -font tkconfixed -borderwidth 0 -padx 0 -pady 0
	    set cw [font measure tkconfixed "0"]
	    set ch [font metrics tkconfixed -linespace]
	    set sw [winfo screenwidth $con]
	    set sh [winfo screenheight $con]
	    # We need the magic hard offsets until I find a way to
	    # correctly assume size
	    if {$cw*($OPT(cols)+2) > $sw} {
		set OPT(cols) [expr {($sw / $cw) - 2}]
	    }
	    if {$ch*($OPT(rows)+3) > $sh} {
		set OPT(rows) [expr {($sh / $ch) - 3}]
	    }
	    # Place it so that the titlebar underlaps the CE titlebar
	    wm geometry $PRIV(root) +0+0
	}
    }
    $con configure -height $OPT(rows) -width $OPT(cols)

    foreach col {prompt stdout stderr stdin proc debug_string} {
	$con tag configure $col -foreground $COLOR($col)
    }
    $con tag configure var -background $COLOR(var)
    $con tag raise sel
    $con tag configure blink -background $COLOR(blink)
    $con tag configure find -background $COLOR(blink)

    set ATTACH($con) [Attach]
    set rb [radiobutton $PRIV(tabframe).cb[winfo name $con] -takefocus 0 \
		-textvariable ::tkcon::ATTACH($con) \
		-selectcolor white -relief sunken \
		-indicatoron 0 -padx 0 -pady 0 -borderwidth 1 \
		-variable ::tkcon::PRIV(curtab) -value $con \
		-command [list ::tkcon::GotoTab $con]]
    if {$::tcl_version >= 8.4} {
	$rb configure -offrelief flat -overrelief raised
    }
    if {$PRIV(AQUA)} {
	$rb configure -padx 4 -pady 4 -highlightthickness 0
    }
    grid $rb -row 0 -column [lindex [grid size $PRIV(tabframe)] 0] -sticky ns
    grid $con -row 1 -column 1 -sticky news

    lappend PRIV(tabs) $con
    return $con
}

proc ::tkcon::GotoTab {con} {
    variable PRIV
    variable ATTACH

    set numtabs [llength $PRIV(tabs)]
    #if {$numtabs == 1} { return }

    if {[regexp {^[0-9]+$} $con]} {
	set curtab [lsearch -exact $PRIV(tabs) $PRIV(console)]
	set nexttab [expr {$curtab + $con}]
	if {$nexttab >= $numtabs} {
	    set nexttab 0
	} elseif {$nexttab < 0} {
	    set nexttab "end"
	}
	set con [lindex $PRIV(tabs) $nexttab]
    } elseif {$con == $PRIV(console)} {
	return
    }

    # adjust console
    if {[winfo exists $PRIV(console)]} {
	lower $PRIV(console)
	$PRIV(console) configure -yscrollcommand {}
	set ATTACH($PRIV(console)) [Attach]
    }
    set PRIV(console) $con
    $con configure -yscrollcommand [list $PRIV(scrolly) set]
    $PRIV(scrolly) configure -command [list $con yview]

    # adjust attach
    eval [linsert $ATTACH($con) 0 Attach]

    set PRIV(curtab) $con

    raise $con

    if {[$con compare 1.0 == end-1c]} {
	Prompt
    }

    # set StatusCursor
    set PRIV(StatusCursor) [$con index insert]

    focus -force $con
}


# Returns text widget of current console. It is suitable e.g.
# for calling write-code-to-pass-to-loc with the pattern
# apply {$w ...} [::tkcon::CurrentConsole]
proc ::tkcon::CurrentConsole {} {
    variable PRIV
    return $PRIV(console)
}

## FIXME rename TopLevel to Toplevel everywhere.
proc ::tkcon::ConsoleToplevelWindow {} {
    variable PRIV
    return $PRIV(root)
}
proc ::tkcon::FocusConsole {} {
    variable PRIV
    set w [ConsoleToplevelWindow]
    set txt $PRIV(console)
    ::gui_util::FocusWindowByName $w $txt
}


proc ::tkcon::NewTab {{con {}}} {
    variable PRIV
    variable ATTACH

    set con   [InitTab $PRIV(base)]
    set slave [GetSlave]
    InitSlave $slave
    $slave alias exit ::tkcon::DeleteTab $con $slave
    if {$PRIV(name) != ""} {
	set ATTACH($con) [list [list $PRIV(name) $slave] slave]
    } else {
	set ATTACH($con) [list $slave slave]
    }
    $PRIV(X) configure -state normal
    MenuConfigure Console "Delete Tab" -state normal
    GotoTab $con
}

# The extra code arg is for the alias of exit to this function
proc ::tkcon::DeleteTab {{con {}} {slave {}} {code 0}} {
    variable PRIV

    set numtabs [llength $PRIV(tabs)]
    if {$numtabs <= 2} {
	$PRIV(X) configure -state disabled
	MenuConfigure Console "Delete Tab" -state disabled
    }
    if {$numtabs == 1} {
	# in the master, it should do the right thing
	# currently the first master still exists - need rearch to fix
	exit
	# we might end up here, depending on how exit is rerouted
	return
    }

    if {$con == ""} {
	set con $PRIV(console)
    }
    catch {unset ATTACH($con)}
    set curtab  [lsearch -exact $PRIV(tabs) $con]
    set PRIV(tabs) [lreplace $PRIV(tabs) $curtab $curtab]

    set numtabs [llength $PRIV(tabs)]
    set nexttab $curtab
    if {$nexttab >= $numtabs} {
	set nexttab end
    }
    set nexttab [lindex $PRIV(tabs) $nexttab]

    GotoTab $nexttab

    if {$slave != "" && $slave != $::tkcon::OPT(exec)} {
	interp delete $slave
    }
    destroy $PRIV(tabframe).cb[winfo name $con]
    destroy $con
}

## ::tkcon::GarbageCollect - do various cleanup ops periodically to our setup
##
proc ::tkcon::GarbageCollect {} {
    variable OPT
    variable PRIV

    foreach w $PRIV(tabs) {
	if {[winfo exists $w]} {
	    ## Remove error tags that no longer span anything
	    ## Make sure the tag pattern matches the unique tag prefix
	    foreach tag [$w tag names] {
		if {[string match _tag* $tag]
		    && ![llength [$w tag ranges $tag]]} {
		    $w tag delete $tag
		}
	    }
	}
    }
    if {$OPT(gc-delay)} {
	after $OPT(gc-delay) ::tkcon::GarbageCollect
    }
}

## ::tkcon::UniqueTag - creates a uniquely named tag, reusing names
## Called by ::tkcon::EvalCmd
# ARGS:	w	- text widget
# Outputs:	tag name guaranteed unique in the widget
## 
proc ::tkcon::UniqueTag {w} {
    set tags [$w tag names]
    set idx 0
    while {[lsearch -exact $tags _tag[incr idx]] != -1} {}
    return _tag$idx
}

## ::tkcon::About - gives about info for tkcon
## 
proc ::tkcon::About {} {
    variable OPT
    variable PRIV
    variable COLOR

    set w $PRIV(base).about
    if {![winfo exists $w]} {
	global tk_patchLevel tcl_patchLevel tcl_version
	toplevel $w
	wm withdraw $w
	wm transient $w $PRIV(root)
	wm group $w $PRIV(root)
	catch {wm attributes $w -type dialog}
	wm title $w "About clcon v$PRIV(version)"
	wm resizable $w 0 0
	button $w.b -text Dismiss -command [list wm withdraw $w]
	text $w.text -height 9 -width 60 \
		-foreground $COLOR(stdin) \
		-background $COLOR(bg) \
		-font $OPT(font) -borderwidth 1 -highlightthickness 0
	grid $w.text -sticky news
	grid $w.b -sticky se -padx 6 -pady 4
	$w.text tag config center -justify center
	$w.text tag config title -justify center -font {Courier -18 bold}
	$w.text insert 1.0 "About clcon v$PRIV(version)" title \
		"\n\nCopyright 1995-2002 Jeffrey Hobbs \
		\nCopyright 2015 Denis Budyak \
		\n\nRelease Info: v$PRIV(version) \
		\nDocumentation and source available at:\n$PRIV(docs)\
		\nUsing: Tcl v$tcl_patchLevel / Tk v$tk_patchLevel" center
	$w.text config -state disabled
	bind $w <Escape> [list destroy $w]
    }
    wm deiconify $w
}

## ::tkcon::InitMenus - inits the menubar and popup for the console
# ARGS:	w	- console text widget
## 
proc ::tkcon::InitMenus {w title} {
    variable OPT
    variable PRIV
    variable COLOR
    global tcl_platform

    if {[catch {menu $w.pop}]} {
	label $w.label -text "Menus not available in plugin mode"
	grid $w.label -sticky ew
	return
    }
    menu $w.context -disabledforeground $COLOR(disabled)
    set PRIV(context) $w.context
    set PRIV(popup) $w.pop

    proc MenuButton {w m l} {
	$w add cascade -label $m -underline 0 -menu $w.$l
	return $w.$l
    }
    proc MenuConfigure {m l args} {
	variable PRIV
	eval [list $PRIV(menubar).[string tolower $m] entryconfigure $l] $args
	eval [list $PRIV(popup).[string tolower $m] entryconfigure $l] $args
    }

    foreach m [list 1.File 2.Console 3.Edit 4.Interp 5.Prefs 6.History 7.Window 8.Help] {
 	set l [string tolower [string range $m 2 end]]
 	MenuButton $w $m $l
 	$w.pop add cascade -label $m -underline 0 -menu $w.pop.$l
    }

    ## File Menu
    ##
    foreach m [list [menu $w.file -disabledforeground $COLOR(disabled)] \
                   [menu $w.pop.file -disabledforeground $COLOR(disabled)]] {
        
        set cmd ::edt::EditNewFile
        $m add command -label "1.Edit new file" -underline 0 -command $cmd

        set cmd [list ::tkcon::OpenForEdit $w "" ""]
	$m add command -label "Open for edit" -command $cmd -accel "Control-O"
        ::clcon_key::b bind TkConsoleTextOverrides <Control-Key-o> "$cmd; break"
        
	$m add command -label "2.Load Tcl File" -underline 0 -command ::tkcon::Load
	$m add cascade -label "3.Save console output..."  -underline 0 -menu $m.save

	$m add separator

        set cmd ::tkcon::ReloadSomeIDESources
	$m add command -label "4.Reload some of IDE sources" -underline 0 \
	    -command $cmd

	## Save Menu
	##
	set s $m.save
	menu $s -disabledforeground $COLOR(disabled)
	$s add command -label "All"	-underline 0 \
		-command {::tkcon::Save {} all}
	$s add command -label "History"	-underline 0 \
		-command {::tkcon::Save {} history}
	$s add command -label "Stdin"	-underline 3 \
		-command {::tkcon::Save {} stdin}
	$s add command -label "Stdout"	-underline 3 \
		-command {::tkcon::Save {} stdout}
	$s add command -label "Stderr"	-underline 3 \
            -command {::tkcon::Save {} stderr}


        ## Recent menu
        set s $m.recent
        menu $s -disabledforeground $COLOR(disabled) -postcommand [list ::recent::RecentMenu $m]
 	$m add cascade -label "5.Open recent ..." -underline 0 -underline 0 -menu $s

	$m add separator
	$m add command -label "Quit" -command ::tkcon::Destroy -accel "Control-W"
    }
        
    ## Console Menu
    ##
    foreach m [list [menu $w.console -disabledfore $COLOR(disabled)] \
	    [menu $w.pop.console -disabledfore $COLOR(disabled)]] {
	$m add command -label "$title Console"	-state disabled

        $m add command -label "1.Attach to SWANK" -underline 0 -command "::tkcon::OuterNewSwank"
        $m add command -label "2.Disconnect from SWANK" -underline 0 -command "::tkcon::DisconnectFromSwank"
        set WarnOnClear {"I dont know if we are in the evaluation now.\
 I'll print a prompt for you, but if your SWANK stopped\
 responding, this will not make it work. In this case try reconnecting\
 to SWANK or wait for your evaluation to return normally"}

        # This is a FIXME. Best way to fix is not to print prompt at all and to just collect all characters which user prints. Completion and other context-dependent actions will be disbled until prompt occurs.
        
	$m add command -label "3.Clear Console" \
            -underline 0 \
            -command "\
  clear; \
  puts stderr $WarnOnClear ; \
  ::tkcon::Prompt"
        
	$m add separator
        
	$m add cascade -label "Tkcon console menu (defunct) ..." -underline 0 -menu $m.tkcon_console
	set su [menu $m.tkcon_console -disabledforeground $COLOR(disabled)]
        
	$su add command -label "New Console" -underline 0 -accel $PRIV(ACC)N \
            -command ::tkcon::New
	$su add command -label "New Tab" -underline 4 -accel $PRIV(ACC)T \
            -command ::tkcon::NewTab
	$su add command -label "Delete Tab" -underline 0 \
            -command ::tkcon::DeleteTab -state disabled
	$su add command -label "Close Console" -underline 0 -accel $PRIV(ACC)w \
            -command ::tkcon::Destroy

	$m add cascade -label "Attach To ... (defunct)" -underline 0 -menu $su.attach

	## Attach Menu
	##
	set sub [menu $su.attach -disabledforeground $COLOR(disabled)]
	$sub add cascade -label "Interpreter" -underline 0 -menu $sub.apps
	$sub add cascade -label "Namespace"   -underline 0 -menu $sub.name

	## Attach Console Menu
	##
	menu $sub.apps -disabledforeground $COLOR(disabled) \
		-postcommand [list ::tkcon::AttachMenu $sub.apps]

	## Attach Namespace Menu
	##
	menu $sub.name -disabledforeground $COLOR(disabled) \
		-postcommand [list ::tkcon::NamespaceMenu $sub.name]

	## Attach Socket Menu
	##
	$sub add cascade -label "Socket" -underline 0 -menu $sub.sock
	menu $sub.sock -disabledforeground $COLOR(disabled) \
	    -postcommand [list ::tkcon::SocketMenu $sub.sock]

	if {[tk windowingsystem] eq "x11"} {
	    ## Attach Display Menu
	    ##
	    $sub add cascade -label "Display" -underline 0 -menu $sub.disp
	    menu $sub.disp -disabledforeground $COLOR(disabled) \
		    -postcommand [list ::tkcon::DisplayMenu $sub.disp]
	}
    }

    ## Edit Menu
    ##
    set text $PRIV(console)
    foreach m [list [menu $w.edit] [menu $w.pop.edit]] {
	$m add command -label "Cut"   -underline 2 -accel $PRIV(ACC)x \
		-command [list ::tkcon::Cut $text]
	$m add command -label "Copy"  -underline 0 -accel $PRIV(ACC)c \
		-command [list ::tkcon::Copy $text]
	$m add command -label "Paste" -underline 0 -accel $PRIV(ACC)v \
		 -command [list ::tkcon::Paste $text]
	$m add separator

        set cmd [list ::fndrpl::OpenFindBox $text "text" "find" {}]
	$m add command -label "Find"  -underline 0 -accel $PRIV(ACC)F \
            -command $cmd
        ::clcon_key::b bind TkConsole <Control-Key-f> $cmd

        set cmd [list ::fndrpl::FindIt $text]
	$m add command -label "Find again"  -underline 0 -accel "F3" -command $cmd 
        bind TkConsole <F3> $cmd

        $m add separator
        
        set cmd "event generate $text <<TkCon_ExpandLisp>>; break"
        $m add command -label "Lisp complete" -accel "Tab, Control-Alt-i" -command $cmd

        set cmd "event generate $text <<TkCon_LispFindDefinition>>; break"
        $m add command -label "Lisp find definition" -accel "Control-." -command $cmd

        $m add separator
        
        set cmd "event generate $text  <<TkCon_ExpandTcl>>; break"
        $m add command -label "Tcl complete" -accel "Control-Alt-u" -command $cmd

        set cmd "event generate $text <<TkCon_TclFindDefinition>>; break"
        $m add command -label "Tcl find definition" -accel "Control-F9" -command $cmd
    }

    ## Interp Menu
    ##
    foreach m [list $w.interp $w.pop.interp] {
	menu $m -disabledforeground $COLOR(disabled) \
            -postcommand [list ::tkcon::InterpMenu $m]
    }

    ## Prefs Menu
    ##
    foreach m [list [menu $w.prefs] [menu $w.pop.prefs]] {

        if {$::tkcon::OPT(putd-output-file) ne {}} {
            set PutdEnabledState normal
        } else {
            set PutdEnabledState disabled
        }           
	$m add check -label "1.Putd-enabled" \
		-underline 0 -variable ::tkcon::OPT(putd-enabled) -state $PutdEnabledState
	$m add check -label "2.Oduvan-backend" \
		-underline 0 -variable ::tkcon::OPT(oduvan-backend)
	$m add check -label "3.Non-Tcl Attachments (defunct)" \
		-underline 0 -variable ::tkcon::OPT(nontcl)
	$m add check -label "4.Show Multiple Matches" \
		-underline 0 -variable ::tkcon::OPT(showmultiple)
	$m add check -label "5.Show Statusbar" \
	    -underline 5 -variable ::tkcon::OPT(showstatusbar) \
	    -command {
		if {$::tkcon::OPT(showstatusbar)} {
		    grid $::tkcon::PRIV(statusbar)
		} else { grid remove $::tkcon::PRIV(statusbar) }
	    }
	$m add cascade -label "6.Scrollbar" -underline 2 -menu $m.scroll

	## Scrollbar Menu
	##
	set m [menu $m.scroll]
	$m add radio -label "Left" -value left \
		-variable ::tkcon::OPT(scrollypos) \
		-command { grid configure $::tkcon::PRIV(scrolly) -column 0 }
	$m add radio -label "Right" -value right \
		-variable ::tkcon::OPT(scrollypos) \
		-command { grid configure $::tkcon::PRIV(scrolly) -column 2 }
    }

    ## History Menu
    ##
    foreach m [list $w.history $w.pop.history] {
	menu $m -disabledforeground $COLOR(disabled) \
		-postcommand [list ::tkcon::HistoryMenu $m]
    }

    ## Window Menu
    ##
    set m $w.window
    menu $m -disabledforeground $COLOR(disabled) \
        -postcommand [list ::window_menu::DynamicWindowMenu $w $m]

    set m $w.pop.window
    menu $m -disabledforeground $COLOR(disabled) \
        -postcommand [list ::window_menu::DynamicWindowMenu $w $m]

    set rr $PRIV(root)
    ::window_menu::WindowMenuKeyBindings $rr $rr $rr
    
    ## Help Menu
    ##
    foreach m [list [menu $w.help] [menu $w.pop.help]] {
	$m add command -label "About " -command ::tkcon::About
	if {![catch {package require ActiveTcl} ver]} {
	    set cmd ""
	    if {$tcl_platform(platform) == "windows"} {
		package require registry
		set ver [join [lrange [split $ver .] 0 3] .]
		set key {HKEY_LOCAL_MACHINE\SOFTWARE\ActiveState\ActiveTcl}
		if {![catch {registry get "$key\\$ver\\Help" ""} help]
		    && [file exists $help]} {
		    set cmd [list exec $::env(COMSPEC) /c start {} $help]
		}
	    } elseif {$tcl_platform(os) == "Darwin"} {
		set ver ActiveTcl-[join [lrange [split $ver .] 0 1] .]
		set rsc "/Library/Frameworks/Tcl.framework/Resources"
		set help "$rsc/English.lproj/$ver/index.html"
		if {[file exists $help]} {
		    set cmd [list exec open $help]
		}
	    } elseif {$tcl_platform(platform) == "unix"} {
		set help [file dirname [info nameofexe]]
		append help /../html/index.html
		if {[file exists $help]} {
		    set cmd [list puts "Start $help"]
		}
	    }
	    if {$cmd != ""} {
		$m add separator
		$m add command -label "ActiveTcl Help" -underline 10 \
		    -command $cmd
	    }
	}
    }
}

## ::tkcon::HistoryMenu - dynamically build the menu for attached interpreters
##
# ARGS:	m	- menu widget
##
proc ::tkcon::HistoryMenu m {
    variable PRIV

    if {![winfo exists $m]} return
    set id [EvalSlave history nextid]
    if {$PRIV(histid)==$id} return
    set PRIV(histid) $id
    $m delete 0 end
    while {($id>1) && ($id>$PRIV(histid)-20) && \
	    ![catch {EvalSlave history event [incr id -1]} tmp]} {
	set lbl $tmp
	if {[string len $lbl]>60} { set lbl [string range $tmp 0 58]... }
	$m add command -label "$id: $lbl" -command "
	$::tkcon::PRIV(console) delete limit end
	$::tkcon::PRIV(console) insert limit [list $tmp]
	$::tkcon::PRIV(console) see end
	::tkcon::Eval $::tkcon::PRIV(console)"
    }
}

## ::tkcon::InterpMenu - dynamically build the menu for attached interpreters
##
# ARGS:	w	- menu widget
##
proc ::tkcon::InterpMenu w {
    variable OPT
    variable PRIV
    variable COLOR

    if {![winfo exists $w]} return
    $w delete 0 end

    $w add command -label "This menu is defunct!!!" -state disabled

    foreach {app type} [Attach] break
    $w add command -label "[string toupper $type]: $app" -state disabled
    if {($OPT(nontcl) && $type eq "interp") || $PRIV(deadapp)} {
	$w add separator
	$w add command -state disabled -label "Communication disabled to"
	$w add command -state disabled -label "dead or non-Tcl interps"
	return
    }
   
    ## Show Last Error
    ##
    $w add separator
    $w add command -label "Show Last Error" \
	    -command [list tkcon error $app $type]

    ## Packages Cascaded Menu
    ##
    $w add separator
    $w add command -label "Manage Packages" -underline 0 \
	-command [list ::tkcon::InterpPkgs $app $type] \
        -state disabled
    # state disabled added by budden

    ## Init Interp
    ##
    $w add separator
    $w add command -label "Send tkcon Commands" \
        -command [list ::tkcon::InitInterp $app $type] \
        -state disabled
    # state disabled added by budden
}

## ::tkcon::PkgMenu - fill in  in the applications sub-menu
## with a list of all the applications that currently exist.
##
proc ::tkcon::InterpPkgs {app type} {
    variable PRIV

    set t $PRIV(base).interppkgs
    if {![winfo exists $t]} {
	toplevel $t
	wm withdraw $t
	wm title $t "$app Packages"
	wm transient $t $PRIV(root)
	wm group $t $PRIV(root)
	catch {wm attributes $t -type dialog}
	bind $t <Escape> [list destroy $t]

	label $t.ll -text "Loadable:" -anchor w
	label $t.lr -text "Loaded:" -anchor w
	listbox $t.loadable -font tkconfixed -background white -borderwidth 1 \
	    -yscrollcommand [list $t.llsy set] -selectmode extended
	listbox $t.loaded -font tkconfixed -background white -borderwidth 1 \
	    -yscrollcommand [list $t.lrsy set]
	scrollbar $t.llsy -command [list $t.loadable yview]
	scrollbar $t.lrsy -command [list $t.loaded yview]
	button $t.load -borderwidth 1 -text ">>" \
	    -command [list ::tkcon::InterpPkgLoad $app $type $t.loadable]
	if {$::tcl_version >= 8.4} {
	    $t.load configure -relief flat -overrelief raised
	}

	set f [frame $t.btns]
	button $f.refresh -width 8 -text "Refresh" -command [info level 0]
	button $f.dismiss -width 8 -text "Dismiss" -command [list destroy $t]
	grid $f.refresh $f.dismiss -padx 4 -pady 3 -sticky ew
	if {$PRIV(AQUA)} { # corner resize control space
	    grid columnconfigure $f [lindex [grid size $f] 0] -minsize 16
	}

	grid $t.ll x x $t.lr x -sticky ew
	grid $t.loadable $t.llsy $t.load $t.loaded $t.lrsy -sticky news
	grid $t.btns -sticky e -columnspan 5
	grid columnconfigure $t {0 3} -weight 1
	grid rowconfigure $t 1 -weight 1
	grid configure $t.load -sticky ""

	bind $t.loadable <Double-1> "[list $t.load invoke]; break"
    }
    $t.loaded delete 0 end
    $t.loadable delete 0 end

    # just in case stuff has been added to the auto_path
    # we have to make sure that the errorInfo doesn't get screwed up
    EvalAttached {
	set __tkcon_error $errorInfo
	catch {package require bogus-package-name}
	set errorInfo ${__tkcon_error}
	unset __tkcon_error
    }
    # get all packages loaded into current interp
    foreach pkg [EvalAttached [list info loaded {}]] {
	set pkg [lindex $pkg 1]
	set loaded($pkg) [package provide $pkg]
    }
    # get all package names currently visible
    foreach pkg [lremove [EvalAttached {package names}] Tcl] {
	set version [EvalAttached [list package provide $pkg]]
	if {$version ne ""} {
	    set loaded($pkg) $version
	} elseif {![info exists loaded($pkg)]} {
	    set loadable($pkg) package
	}
    }
    # get packages that are loaded in any interp
    foreach pkg [EvalAttached {info loaded}] {
	set pkg [lindex $pkg 1]
	if {![info exists loaded($pkg)] && ![info exists loadable($pkg)]} {
	    set loadable($pkg) load
	}
    }
    foreach pkg [lsort -dictionary [array names loadable]] {
	foreach v [EvalAttached [list package version $pkg]] {
	    $t.loadable insert end [list $pkg $v "($loadable($pkg))"]
	}
    }
    foreach pkg [lsort -dictionary [array names loaded]] {
	$t.loaded insert end [list $pkg $loaded($pkg)]
    }

    wm deiconify $t
    raise $t
}

proc ::tkcon::InterpPkgLoad {app type lb} {
    # load the lb entry items into the interp
    foreach sel [$lb curselection] {
	foreach {pkg ver method} [$lb get $sel] { break }
	if {$method == "(package)"} {
	    set code [catch {::tkcon::EvalOther $app $type \
				 package require $pkg $ver} msg]
	} elseif {$method == "(load)"} {
	    set code [catch {::tkcon::EvalOther $app $type load {} $pkg} msg]
	} else {
	    set code 1
	    set msg "Incorrect entry in Loadable selection"
	}
	if {$code} {
	    tk_messageBox -icon error -title "Error requiring $pkg" -type ok \
		-message "Error requiring $pkg $ver:\n$msg\n$::errorInfo"
	}
    }
    # refresh package list
    InterpPkgs $app $type
}

## ::tkcon::AttachMenu - fill in  in the applications sub-menu
## with a list of all the applications that currently exist.
##
proc ::tkcon::AttachMenu m {
    variable OPT
    variable PRIV

    array set interps [set tmp [Interps]]
    foreach {i j} $tmp { set tknames($j) {} }

    $m delete 0 end
    set cmd {::tkcon::RePrompt \n [::tkcon::CmdGet $::tkcon::PRIV(console)]}
    $m add radio -label {None (use local slave) } -accel $PRIV(ACC)1 \
	    -variable ::tkcon::PRIV(app) \
	    -value [concat $::tkcon::PRIV(name) $::tkcon::OPT(exec)] \
	    -command "::tkcon::Attach {}; $cmd"
    $m add separator
    $m add command -label "Foreign Tk Interpreters" -state disabled
    foreach i [lsort [lremove [::send::interps] [array names tknames]]] {
	$m add radio -label $i -variable ::tkcon::PRIV(app) -value $i \
		-command "::tkcon::Attach [list $i] interp; $cmd"
    }
    $m add separator

    $m add command -label "tkcon Interpreters" -state disabled
    foreach i [lsort [array names interps]] {
	if {$interps($i) eq ""} { set interps($i) "no Tk" }
	if {[regexp {^Slave[0-9]+} $i]} {
	    set opts [list -label "$i ($interps($i))" \
		    -variable ::tkcon::PRIV(app) -value $i \
		    -command "::tkcon::Attach [list $i] slave; $cmd"]
	    if {$PRIV(name) eq $i} {
		append opts " -accel $PRIV(ACC)2"
	    }
	    eval [list $m add radio] $opts
	} else {
	    set name [concat Main $i]
	    if {$name eq "Main"} {
		$m add radio -label "$name ($interps($i))" -accel $PRIV(ACC)3 \
			-variable ::tkcon::PRIV(app) -value Main \
			-command "::tkcon::Attach [list $name] slave; $cmd"
	    } else {
		$m add radio -label "$name ($interps($i))" \
			-variable ::tkcon::PRIV(app) -value $i \
			-command "::tkcon::Attach [list $name] slave; $cmd"
	    }
	}
    }
}

## Displays Cascaded Menu
##
proc ::tkcon::DisplayMenu m {
    $m delete 0 end
    set cmd {::tkcon::RePrompt \n [::tkcon::CmdGet $::tkcon::PRIV(console)]}

    $m add command -label "New Display" -command ::tkcon::NewDisplay
    foreach disp [Display] {
	$m add separator
	$m add command -label $disp -state disabled
	set res [Display $disp]
	set win [lindex $res 0]
	foreach i [lsort [lindex $res 1]] {
	    $m add radio -label $i -variable ::tkcon::PRIV(app) -value $i \
		    -command "::tkcon::Attach [list $i] [list dpy:$win]; $cmd"
	}
    }
}

## Sockets Cascaded Menu
##
proc ::tkcon::SocketMenu m {
    $m delete 0 end
    set cmd {::tkcon::RePrompt \n [::tkcon::CmdGet $::tkcon::PRIV(console)]}

    $m add command -label "Create Connection" \
	    -command "::tkcon::NewSocket; $cmd"
    foreach sock [file channels sock*] {
	$m add radio -label $sock -variable ::tkcon::PRIV(app) -value $sock \
		-command "::tkcon::Attach $sock socket; $cmd"
    }
}

## Namepaces Cascaded Menu
##
proc ::tkcon::NamespaceMenu m {
    variable PRIV
    variable OPT

    $m delete 0 end
    if {($PRIV(deadapp) || $PRIV(apptype) eq "socket" || \
	    ($OPT(nontcl) && $PRIV(apptype) eq "interp"))} {
	$m add command -label "No Namespaces" -state disabled
	return
    }

    ## Same command as for ::tkcon::AttachMenu items
    set cmd {::tkcon::RePrompt \n [::tkcon::CmdGet $::tkcon::PRIV(console)]}

    set names [lsort [Namespaces ::]]
    if {[llength $names] > $OPT(maxmenu)} {
	$m add command -label "Attached to $PRIV(namesp)" -state disabled
	$m add command -label "List Namespaces" \
		-command [list ::tkcon::NamespacesList $names]
    } else {
	foreach i $names {
	    if {$i eq "::"} {
		$m add radio -label "Main" -value $i \
			-variable ::tkcon::PRIV(namesp) \
			-command "::tkcon::AttachNamespace [list $i]; $cmd"
	    } else {
		$m add radio -label $i -value $i \
			-variable ::tkcon::PRIV(namesp) \
			-command "::tkcon::AttachNamespace [list $i]; $cmd"
	    }
	}
    }
}

## Namepaces List 
##
proc ::tkcon::NamespacesList {names} {
    variable PRIV

    set f $PRIV(base).namespaces
    catch {destroy $f}
    toplevel $f
    catch {wm attributes $f -type dialog}
    listbox $f.names -width 30 -height 15 -selectmode single \
	-yscrollcommand [list $f.scrollv set] \
	-xscrollcommand [list $f.scrollh set] \
	-background white -borderwidth 1
    scrollbar $f.scrollv -command [list $f.names yview]
    scrollbar $f.scrollh -command [list $f.names xview] -orient horizontal
    frame $f.buttons
    button $f.cancel -text "Cancel" -command [list destroy $f]

    grid $f.names $f.scrollv -sticky nesw
    grid $f.scrollh -sticky ew
    grid $f.buttons -sticky nesw
    grid $f.cancel -in $f.buttons -pady 6

    grid columnconfigure $f 0 -weight 1
    grid rowconfigure $f  0 -weight 1
    #fill the listbox
    foreach i $names {
	if {$i eq "::"} {
	    $f.names insert 0 Main
	} else {
	    $f.names insert end $i
	}
    }
    #Bindings
    bind $f.names <Double-1> {
	## Catch in case the namespace disappeared on us
	catch { ::tkcon::AttachNamespace [%W get [%W nearest %y]] }
	::tkcon::RePrompt "\n" [::tkcon::CmdGet $::tkcon::PRIV(console)]
	destroy [winfo toplevel %W]
    }
}

## ::tkcon::Attach - called to attach tkcon to an interpreter
# ARGS:	name	- application name to which tkcon sends commands
#		  This is either a slave interperter name or tk appname.
#	type	- (slave|interp) type of interpreter we're attaching to
#		  slave means it's a tkcon interpreter
#		  interp means we'll need to 'send' to it.
# Results:	::tkcon::EvalAttached is recreated to evaluate in the
#		appropriate interpreter
##
proc ::tkcon::Attach {{name <NONE>} {type slave} {ns {}}} {
    variable PRIV
    variable OPT
    variable ATTACH

    if {[llength [info level 0]] == 1} {
	# no args were specified, return the attach info instead
	return [AttachId]
    }
    set path [concat $PRIV(name) $OPT(exec)]

    set PRIV(displayWin) .
    if {$type eq "namespace"} {
	return [uplevel 1 ::tkcon::AttachNamespace $name]
    } elseif {[string match dpy:* $type]} {
	set PRIV(displayWin) [string range $type 4 end]
    } elseif {[string match sock* $type]} {
	global tcl_version
	if {[catch {eof $name} res]} {
	    return -code error "No known channel \"$name\""
	} elseif {$res} {
	    catch {close $name}
	    return -code error "Channel \"$name\" returned EOF"
	}
	set app $name
	set type socket
    } elseif {$name ne ""} {
	array set interps [Interps]
	if {[string match {[Mm]ain} [lindex $name 0]]} {
	    set name [lrange $name 1 end]
	}
	if {$name eq $path} {
	    set name {}
	    set app $path
	    set type slave
	} elseif {[info exists interps($name)]} {
	    if {$name eq ""} { set name Main; set app Main }
	    set type slave
	} elseif {[interp exists $name]} {
	    set name [concat $PRIV(name) $name]
	    set type slave
	} elseif {[interp exists [concat $OPT(exec) $name]]} {
	    set name [concat $path $name]
	    set type slave
	} elseif {[lsearch -exact [::send::interps] $name] > -1} {
	    if {[EvalSlave info exists tk_library] 
		&& $name eq [EvalSlave tk appname]} {
		set name {}
		set app $path
		set type slave
	    } elseif {[set i [lsearch -exact \
		    [Main set ::tkcon::PRIV(interps)] $name]] != -1} {
		set name [lindex [Main set ::tkcon::PRIV(slaves)] $i]
		if {[string match {[Mm]ain} $name]} { set app Main }
		set type slave
	    } else {
		set type interp
	    }
	} else {
	    return -code error "No known interpreter \"$name\""
	}
    } else {
	set app $path
    }
    if {![info exists app]} { set app $name }
    array set PRIV [list app $app appname $name apptype $type deadapp 0]

    ## ::tkcon::EvalAttached - evaluates the args in the attached interp
    ## args should be passed to this procedure as if they were being
    ## passed to the 'eval' procedure.  This procedure is dynamic to
    ## ensure evaluation occurs in the right interp.
    # ARGS:	args	- the command and args to evaluate
    ##
    set PRIV(namesp) ::
    set namespOK 0
    switch -glob -- $type {
	slave {
	    if {$name eq ""} {
		interp alias {} ::tkcon::EvalAttached {} \
			::tkcon::EvalSlave uplevel \#0
	    } elseif {$PRIV(app) eq "Main"} {
		interp alias {} ::tkcon::EvalAttached {} ::tkcon::Main
	    } elseif {$PRIV(name) eq $PRIV(app)} {
		interp alias {} ::tkcon::EvalAttached {} uplevel \#0
	    } else {
		interp alias {} ::tkcon::EvalAttached {} \
			::tkcon::Slave $::tkcon::PRIV(app)
	    }
	    set namespOK 1
	}
	sock* {
	    interp alias {} ::tkcon::EvalAttached {} \
		    ::tkcon::EvalSlave uplevel \#0
	    # The file event will just puts whatever data is found
	    # into the interpreter
	    fconfigure $name -buffering line -blocking 0
	    fileevent $name readable [list ::tkcon::EvalSocketEvent $name]
	}
	dpy:* -
	interp {
	    if {$OPT(nontcl)} {
		interp alias {} ::tkcon::EvalAttached {} ::tkcon::EvalSlave
	    } else {
                tr "Surprise"
		interp alias {} ::tkcon::EvalAttached {} ::tkcon::EvalSend
		set namespOK 1
	    }
	}
	default {
	    return -code error "[lindex [info level 0] 0] did not specify\
		    a valid type: must be slave or interp"
	}
    }
    if {$ns ne "" && $namespOK} {
	AttachNamespace $ns
    }
    return [AttachId]
}


proc ::tkcon::AttachId {} {
    # return Attach info in a form that Attach accepts again
    variable PRIV

    if {$PRIV(appname) eq ""} {
	variable OPT
	set appname [concat $PRIV(name) $OPT(exec)]
    } else {
	set appname $PRIV(appname)
    }
    set id [list $appname $PRIV(apptype)]
    # only display ns info if it isn't "::" as that is what is also
    # used to indicate no eval in namespace
    if {$PRIV(namesp) ne "::"} { lappend id $PRIV(namesp) }
    if {[info exists PRIV(console)]} {
	variable ATTACH
	set ATTACH($PRIV(console)) $id
    }
    return $id
}

## ::tkcon::AttachNamespace - called to attach tkcon to a namespace
# ARGS:	name	- namespace name in which tkcon should eval commands
# Results:	::tkcon::EvalAttached will be modified
##
proc ::tkcon::AttachNamespace { name } {
    variable PRIV
    variable OPT

    # We could enable 'socket' bound Tcl interps, but we'd have to create
    # a return listening socket
    if {($OPT(nontcl) && $PRIV(apptype) eq "interp")
	|| $PRIV(apptype) eq "socket"
	|| $PRIV(deadapp)} {
	return -code error "can't attach to namespace in attached environment"
    }
    if {$name eq "Main"} {set name ::}
    if {$name ne "" && [lsearch [Namespaces ::] $name] == -1} {
	return -code error "No known namespace \"$name\""
    }
    if {[regexp {^(|::)$} $name]} {
	## If name=={} || ::, we want the primary namespace
	set alias [interp alias {} ::tkcon::EvalAttached]
	if {[string match ::tkcon::EvalNamespace* $alias]} {
	    eval [list interp alias {} ::tkcon::EvalAttached {}] \
		    [lindex $alias 1]
	}
	set name ::
    } else {
	interp alias {} ::tkcon::EvalAttached {} ::tkcon::EvalNamespace \
		[interp alias {} ::tkcon::EvalAttached] [list $name]
    }
    set PRIV(namesp) $name
    return [AttachId]
}

## ::tkcon::NewSocket - called to create a socket to connect to
# ARGS:	none
# Results:	It will create a socket, and attach if requested
##
proc ::tkcon::NewSocket {} {
    variable PRIV

    set t $PRIV(base).newsock
    if {![winfo exists $t]} {
	toplevel $t
	wm withdraw $t
	catch {wm attributes $t -type dialog}
	wm title $t "tkcon Create Socket"
	wm resizable $t 1 0
	label $t.lhost -text "Host: "
	entry $t.host -width 16 -takefocus 1
	label $t.lport -text "Port: "
	entry $t.port -width 4 -takefocus 1
	button $t.ok -text "OK" -command {set ::tkcon::PRIV(grab) 1} -width 4 \
	    -takefocus 1
	bind $t.host <Return> [list focus $t.port]
	bind $t.port <Return> [list focus $t.ok]
	bind $t.ok   <Return> [list $t.ok invoke]
	grid $t.lhost $t.host $t.lport $t.port $t.ok -sticky ew
	grid configure $t.ok -padx 4 -pady 2
	grid columnconfig $t 1 -weight 1
	grid rowconfigure $t 1 -weight 1
	if {$PRIV(AQUA)} { # corner resize control space
	    grid columnconfigure $t [lindex [grid size $t] 0] -minsize 16
	}
	wm transient $t $PRIV(root)
	wm group $t $PRIV(root)
	wm geometry $t +[expr {([winfo screenwidth $t]-[winfo \
		reqwidth $t]) / 2}]+[expr {([winfo \
		screenheight $t]-[winfo reqheight $t]) / 2}]
	bind $t <Escape> [list destroy $t]
    }
    #$t.host delete 0 end
    #$t.port delete 0 end
    wm deiconify $t
    raise $t
    grab $t
    focus $t.host
    vwait ::tkcon::PRIV(grab)
    grab release $t
    wm withdraw $t
    set host [$t.host get]
    set port [$t.port get]
    if {$host == ""} { return }
    if {[catch {
	set sock [socket $host $port]
    } err]} {
	tk_messageBox -title "Socket Connection Error" \
		-message "Unable to connect to \"$host:$port\":\n$err" \
		-icon error -type ok
    } else {
	Attach $sock socket
    }
}


## ::tkcon::Load - sources a file into the console
## The file is actually sourced in the currently attached's interp
# ARGS:	fn	- (optional) filename to source in
# Returns:	selected filename ({} if nothing was selected)
## 
proc ::tkcon::Load { {fn ""} } {
    set types {
	{{Tcl Files}	{.tcl .tk}}
	{{Text Files}	{.txt}}
	{{All Files}	*}
    }
    # Allow for VFS directories, use Tk dialogs automatically when in
    # VFS-based areas
    set check [expr {$fn == "" ? [pwd] : $fn}]
    if {$::tcl_version >= 8.4 && [lindex [file system $check] 0] == "tclvfs"} {
	set opencmd [list ::tk::dialog::file:: open]
    } else {
	set opencmd [list tk_getOpenFile]
    }
    if {$fn eq "" &&
	([catch {tk_getOpenFile -filetypes $types \
		     -title "Source File"} fn] || $fn eq "")
    } { return }
    EvalAttached [list source $fn]
}

proc ::tkcon::DoOpenFileForEdit {fn} {
    ::edt::edit -type file -wrap char -- $fn
    }

## ::tkcon::OpenForEdit 
# ARGS:	
# parent - widget parent for dialog 
# fn - filename to source in, "" means no filename
# initialdir - for tk_getOpenFile, "" means we will not supply it.
# Returns:	selected filename ({} if nothing was selected)
## 
proc ::tkcon::OpenForEdit { parent fn initialdir } {
    set types {
	{{All Files}	*}
	{{Lisp Files}	{.lisp .asd}}
	{{Tcl Files}	{.tcl .tk}}
	{{Text Files}	{.txt}}
    }
    set opencmd [list tk_getOpenFile -parent $parent -filetypes $types \
		     -title "Source File"]
    if {$initialdir ne ""} {
        lappend opencmd "-initialdir" $initialdir
    }
    if {$fn eq "" &&
	([catch { eval $opencmd } fn] || $fn eq "")} { 
       return 
    }
    DoOpenFileForEdit $fn
    return $fn
}

# Let user to choose file name when saving file
proc ::tkcon::GetFileNameForSave {parent type} {
    set savecmd [list tk_getSaveFile]
    set fn ""
    set types {
        {{Lisp Files}   {.lisp .asd}}
        {{Tcl Files}	{.tcl .tk}}
        {{Text Files}	{.txt}}
        {{All Files}	*}
    }
    if {[catch {eval $savecmd [list -filetypes $types          \
                                    -parent $parent            \
                                    -title "Save $type"]} fn]} {
        return ""
    } else {
        return $fn
    }
}


## ::tkcon::Save - saves the console or other widget buffer to a file
## This does not eval in a slave because it's not necessary
# ARGS:	w	- console text widget
# 	fn	- (optional) filename to save to
#       type    - type of content to save
#       opt     - at least for a type eq "widget", a widget
##
proc ::tkcon::Save { {fn ""} {type ""} {opt ""} {mode w} } {
    variable PRIV

    if {![regexp -nocase {^(all|history|stdin|stdout|stderr|widget)$} $type]} {
	array set s { 0 All 1 History 2 Stdin 3 Stdout 4 Stderr 5 Cancel }
	## Allow user to specify what kind of stuff to save
	set type [tk_dialog $PRIV(base).savetype "Save Type" \
		"What part of the text do you want to save?" \
		questhead 0 $s(0) $s(1) $s(2) $s(3) $s(4) $s(5)]
	if {$type == 5 || $type == -1} return
	set type $s($type)
    }
    if {$type eq "widget"} {
        set parent $opt
    } else {
        set parent $PRIV(console)
    }
    if {$fn eq {}} {
        set fn [GetFileNameForSave $parent $type]
        if {$fn eq {}} {
            return
        }
    }
    
    set type [string tolower $type]
    switch $type {
	stdin -	stdout - stderr {
	    set data {}
	    foreach {first last} [$PRIV(console) tag ranges $type] {
		lappend data [$PRIV(console) get $first $last]
	    }
	    set data [join $data \n]
	}
	history		{ set data [tkcon history] }
	all - default	{ set data [$PRIV(console) get 1.0 end-1c] }
	widget		{
	    set data [$opt get 1.0 end-1c]
	}
    }
    if {[catch {open $fn $mode} fid]} {
	return -code error "Save Error: Unable to open '$fn' for writing\n$fid"
    }
    puts -nonewline $fid $data
    close $fid
}

## ::tkcon::MainInit
## This is only called for the main interpreter to include certain procs
## that we don't want to include (or rather, just alias) in slave interps.
##
proc ::tkcon::MainInit {} {
    variable PRIV
    variable OPT

    if {![info exists PRIV(slaves)]} {
	array set PRIV [list slave 0 slaves Main name {} \
		interps [list [tk appname]]]
    }
    interp alias {} ::tkcon::Main {} ::tkcon::InterpEval Main
    interp alias {} ::tkcon::Slave {} ::tkcon::InterpEval

    proc ::tkcon::GetSlave {{slave {}}} {
	set i 0
	while {[Slave $slave [list interp exists Slave[incr i]]]} {
	    # oh my god, an empty loop!
	}
	set interp [Slave $slave [list interp create Slave$i]]

        #interp eval $interp "proc ::CurIntPath {tag} { set result $interp \n if { \$tag ne {} } {             puts \"CurIntPath = \$result\ (\$tag)\" } \n return \$result }"
        interp eval $slave "proc ::CurIntPath {puts} {}"
        interp eval $interp "::CurIntPath {Created at GetSlave}"
        
	return $interp
    }

    ## ::tkcon::New - create new console window
    ## Creates a slave interpreter and sources in this script.
    ## All other interpreters also get a command to eval function in the
    ## new interpreter.
    ## 
    proc ::tkcon::New {} {
	variable PRIV
	global argv0 argc argv

	set tmp [GetSlave]
	lappend PRIV(slaves) $tmp
	load {} Tk $tmp
	# If we have tbcload, then that should be autoloaded into slaves.
	set idx [lsearch [info loaded] "* Tbcload"]
	if {$idx != -1} { catch {load {} Tbcload $tmp} }
	lappend PRIV(interps) [$tmp eval [list tk appname \
		"[tk appname] $tmp"]]
	if {[info exists argv0]} {$tmp eval [list set argv0 $argv0]}
	if {[info exists argc]}  {$tmp eval [list set argc $argc]}
	if {[info exists argv]}  {$tmp eval [list set argv $argv]}
	$tmp eval [list namespace eval ::tkcon {}]
	$tmp eval [list set ::tkcon::PRIV(name) $tmp]
	$tmp eval [list set ::tkcon::PRIV(SCRIPT) $::tkcon::PRIV(SCRIPT)]
	$tmp alias exit				::tkcon::Exit $tmp
	$tmp alias ::tkcon::Destroy		::tkcon::Destroy $tmp
	$tmp alias ::tkcon::New			::tkcon::New
	$tmp alias ::tkcon::GetSlave		::tkcon::GetSlave $tmp
	$tmp alias ::tkcon::Main		::tkcon::InterpEval Main
	$tmp alias ::tkcon::Slave		::tkcon::InterpEval
	$tmp alias ::tkcon::Interps		::tkcon::Interps
	$tmp alias ::tkcon::NewDisplay		::tkcon::NewDisplay
	$tmp alias ::tkcon::Display		::tkcon::Display
	$tmp eval {
	    if [catch {source -rsrc tkcon}] { source $::tkcon::PRIV(SCRIPT) }
	}
	return $tmp
    }

    ## ::tkcon::Exit - full exit OR destroy slave console
    ## This proc should only be called in the main interpreter from a slave.
    ## The master determines whether we do a full exit or just kill the slave.
    ## 
    proc ::tkcon::Exit {slave args} {
	variable PRIV
	variable OPT

	## Slave interpreter exit request
	if {$OPT(slaveexit) eq "exit" || [llength $PRIV(interps)] == 1} {
	    ## Only exit if it specifically is stated to do so, or this
	    ## is the last interp
	    uplevel 1 exit $args
	} else {
	    ## Otherwise we will delete the slave interp and associated data
	    Destroy $slave
	}
    }

    proc ::tkcon::CheckIfEditorAllowsToExitAndSayToUser {} {
        variable PRIV
        if {[::edt::EditorHaveModifiedWindowsP]} {
            tk_messageBox -parent $PRIV(console) -title "Quit clcon" -message "You have unsaved files" 
            ::edt::ShowSomeEditor
            return 0
        } else {
            return 1
        }
    }
    
    ## ::tkcon::Destroy - destroy console window
    ## This proc should only be called by the main interpreter.  If it is
    ## called from there, it will ask before exiting tkcon.  All others
    ## (slaves) will just have their slave interpreter deleted, closing them.
    ## 
    proc ::tkcon::Destroy {{slave {}}} {
	variable PRIV

        if {![::tkcon::CheckIfEditorAllowsToExitAndSayToUser]} {
            return
        }
        
	# Just close on the last one
	if {[llength $PRIV(interps)] == 1} { exit }
        
	if {"" == $slave} {
	    ## Main interpreter close request
	    if {[tk_messageBox -parent $PRIV(root) -title "Quit tkcon?" \
		     -message "Close all windows and exit tkcon?" \
		     -icon question -type yesno] == "yes"} { exit }
	    return
	} elseif {$slave == $::tkcon::OPT(exec)} {
	    set name  [tk appname]
	    set slave "Main"
	} else {
	    ## Slave interpreter close request
	    set name [InterpEval $slave]
	    interp delete $slave
	}
	set PRIV(interps) [lremove $PRIV(interps) [list $name]]
	set PRIV(slaves)  [lremove $PRIV(slaves) [list $slave]]
	StateCleanup $slave
    }

    if {$OPT(overrideexit)} {
	## We want to do a couple things before exiting...
	if {[catch {rename ::exit ::tkcon::FinalExit} err]} {
	    puts stderr "tkcon might panic:\n$err"
	}
	proc ::exit args {
	    if {$::tkcon::OPT(usehistory)} {
		if {[catch {open $::tkcon::PRIV(histfile) w} fid]} {
		    puts stderr "unable to save history file:\n$fid"
		    # pause a moment, because we are about to die finally...
		    after 1000
		} else {
		    set max [::tkcon::EvalSlave history nextid]
		    set id [expr {$max - $::tkcon::OPT(history)}]
		    if {$id < 1} { set id 1 }
		    ## FIX: This puts history in backwards!!
		    while {($id < $max) && ![catch \
			    {::tkcon::EvalSlave history event $id} cmd]} {
			if {$cmd ne ""} {
			    puts $fid "::tkcon::EvalSlave\
				    history add [list $cmd]"
			}
			incr id
		    }
                    ::recent::SaveRecentFilesList $fid
		    close $fid
		}
	    }
	    uplevel 1 ::tkcon::FinalExit $args
	}
    }

    ## ::tkcon::InterpEval - passes evaluation to another named interpreter
    ## If the interpreter is named, but no args are given, it returns the
    ## [tk appname] of that interps master (not the associated eval slave).
    ##
    proc ::tkcon::InterpEval {{slave {}} args} {
	variable PRIV

	if {[llength [info level 0]] == 1} {
	    # no args given
	    return $PRIV(slaves)
	} elseif {[string match {[Mm]ain} $slave]} {
	    set slave {}
	}
	if {[llength $args]} {
	    return [interp eval $slave uplevel \#0 $args]
	} else {
	    # beware safe interps with Tk
	    if {[interp eval $slave {llength [info commands tk]}]} {
		if {[catch {interp eval $slave tk appname} name]} {
		    return "safetk"
		}
		return $name
	    }
	}
    }

    proc ::tkcon::Interps {{ls {}} {interp {}}} {
	if {$interp eq ""} {
	    lappend ls {} [tk appname]
	}
	foreach i [interp slaves $interp] {
	    if {$interp ne ""} { set i "$interp $i" }
	    if {[interp eval $i package provide Tk] ne ""} {
		# beware safe interps with Tk
		if {[catch {interp eval $i tk appname} name]} {
		    set name {}
		}
		lappend ls $i $name
	    } else {
		lappend ls $i {}
	    }
	    set ls [Interps $ls $i]
	}
	return $ls
    }

    proc ::tkcon::Display {{disp {}}} {
	variable DISP

	set res {}
	if {$disp != ""} {
	    if {![info exists DISP($disp)]} { return }
	    return [list $DISP($disp) [winfo interps -displayof $DISP($disp)]]
	}
	return [lsort -dictionary [array names DISP]]
    }

    proc ::tkcon::NewDisplay {} {
	variable PRIV
	variable DISP

	set t $PRIV(base).newdisp
	if {![winfo exists $t]} {
	    toplevel $t
	    wm withdraw $t
	    catch {wm attributes $t -type dialog}
	    wm title $t "tkcon Attach to Display"
	    label $t.gets -text "New Display: "
	    entry $t.data -width 32
	    button $t.ok -text "OK" -command {set ::tkcon::PRIV(grab) 1}
	    bind $t.data <Return> [list $t.ok invoke]
	    bind $t.ok   <Return> [list $t.ok invoke]
	    grid $t.gets $t.data -sticky ew
	    grid $t.ok   -	 -sticky ew
	    grid columnconfig $t 1 -weight 1
	    grid rowconfigure $t 1 -weight 1
	    wm transient $t $PRIV(root)
	    wm geometry $t +[expr {([winfo screenwidth $t]-[winfo \
		    reqwidth $t]) / 2}]+[expr {([winfo \
		    screenheight $t]-[winfo reqheight $t]) / 2}]
	}
	$t.data delete 0 end
	wm deiconify $t
	raise $t
	grab $t
	focus $t.data
	vwait ::tkcon::PRIV(grab)
	grab release $t
	wm withdraw $t
	set disp [$t.data get]
	if {$disp == ""} { return }
	regsub -all {\.} [string tolower $disp] ! dt
	set dt $PRIV(base).$dt
	destroy $dt
	if {[catch {
	    toplevel $dt -screen $disp
	    set interps [winfo interps -displayof $dt]
	    if {![llength $interps]} {
		error "No other Tk interpreters on $disp"
	    }
	    ::send::send -displayof $dt [lindex $interps 0] [list info tclversion]
	} err]} {
	    global env
	    if {[info exists env(DISPLAY)]} {
		set myd $env(DISPLAY)
	    } else {
		set myd "myDisplay:0"
	    }
	    tk_messageBox -title "Display Connection Error" \
		    -message "Unable to connect to \"$disp\":\n$err\
		    \nMake sure you have xauth-based permissions\
		    (xauth add $myd . `mcookie`), and xhost is disabled\
		    (xhost -) on \"$disp\"" \
		    -icon error -type ok
	    destroy $dt
	    return
	}
	set DISP($disp) $dt
	wm withdraw $dt
	bind $dt <Destroy> [subst {catch {unset ::tkcon::DISP($disp)}}]
	tk_messageBox -title "$disp Connection" \
		-message "Connected to \"$disp\", found:\n[join $interps \n]" \
		-type ok
    }

}

## ::tkcon::Event - get history event, search if string != {}
## look forward (next) if $int>0, otherwise look back (prev)
# ARGS:	W	- console widget
##
proc ::tkcon::Event {int {str {}}} {
    if {!$int} return

    variable PRIV
    set w $PRIV(console)

    set nextid [EvalSlave history nextid]
    if {$str ne ""} {
	## String is not empty, do an event search
	set event $PRIV(event)
	if {$int < 0 && $event == $nextid} { set PRIV(cmdbuf) $str }
	set len [string len $PRIV(cmdbuf)]
	incr len -1
	if {$int > 0} {
	    ## Search history forward
	    while {$event < $nextid} {
		if {[incr event] == $nextid} {
		    $w delete limit end
		    $w insert limit $PRIV(cmdbuf)
		    break
		} elseif {
		    ![catch {EvalSlave history event $event} res] &&
		    [set p [string first $PRIV(cmdbuf) $res]] > -1
		} {
		    set p2 [expr {$p + [string length $PRIV(cmdbuf)]}]
		    $w delete limit end
		    $w insert limit $res
		    Blink $w "limit + $p c" "limit + $p2 c"
		    break
		}
	    }
	    set PRIV(event) $event
	} else {
	    ## Search history reverse
	    while {![catch {EvalSlave history event [incr event -1]} res]} {
		if {[set p [string first $PRIV(cmdbuf) $res]] > -1} {
		    set p2 [expr {$p + [string length $PRIV(cmdbuf)]}]
		    $w delete limit end
		    $w insert limit $res
		    set PRIV(event) $event
		    Blink $w "limit + $p c" "limit + $p2 c"
		    break
		}
	    }
	}
    } else {
	## String is empty, just get next/prev event
        putd $PRIV(event)
	if {$int > 0} {
	    ## Goto next command in history
	    if {$PRIV(event) < $nextid} {
		$w delete limit end
		if {[incr PRIV(event)] == $nextid} {
		    $w insert limit $PRIV(cmdbuf)
		} else {
		    $w insert limit [EvalSlave history event $PRIV(event)]
		}
	    } else {
                bell
            }
	} else {
	    ## Goto previous command in history
	    if {$PRIV(event) == $nextid} {
		set PRIV(cmdbuf) [CmdGet $w]
	    }
	    if {[catch {EvalSlave history event [incr PRIV(event) -1]} res]} {
                bell
		incr PRIV(event)
	    } else {
		$w delete limit end
		$w insert limit $res
	    }
	}
    }
    $w mark set insert end
    $w see end
}

## ::tkcon::Highlight - magic highlighting
## beware: voodoo included
# ARGS:
##
proc ::tkcon::Highlight {w type} {
    variable COLOR
    variable OPT

    switch -exact $type {
	"error" { HighlightError $w }
    }
}

## ::tkcon::HighlightError - magic error highlighting
## beware: voodoo included
# ARGS:
##
proc ::tkcon::HighlightError w {
    variable COLOR
    variable OPT

    ## do voodoo here
    set app [Attach]
    # we have to pull the text out, because text regexps are screwed on \n's.
    set info [$w get 1.0 end-1c]
    # Check for specific line error in a proc
    set exp(proc) "\"(\[^\"\]+)\"\n\[\t \]+\\\(procedure \"(\[^\"\]+)\""
    # Check for too few args to a proc
    set exp(param) "parameter \"(\[^\"\]+)\" to \"(\[^\"\]+)\""
    set start 1.0
    while {
	[regexp -indices -- $exp(proc) $info junk what cmd] ||
	[regexp -indices -- $exp(param) $info junk what cmd]
    } {
	foreach {w0 w1} $what {c0 c1} $cmd {break}
	set what [string range $info $w0 $w1]
	set cmd  [string range $info $c0 $c1]
	if {[string match *::* $cmd]} {
	    set res [uplevel 1 ::tkcon::EvalOther $app namespace eval \
		    [list [namespace qualifiers $cmd] \
		    [list info procs [namespace tail $cmd]]]]
	} else {
	    set res [uplevel 1 ::tkcon::EvalOther $app info procs [list $cmd]]
	}
	if {[llength $res]==1} {
	    set tag [UniqueTag $w]
	    $w tag add $tag $start+${c0}c $start+1c+${c1}c
	    $w tag configure $tag -foreground $COLOR(stdout)
	    $w tag bind $tag <Enter> [list $w tag configure $tag -under 1]
	    $w tag bind $tag <Leave> [list $w tag configure $tag -under 0]
	    $w tag bind $tag <ButtonRelease-1> "if {!\$tk::Priv(mouseMoved)} \
		    {[list $OPT(edit) -attach $app -type proc -find $what -- $cmd]}"
	}
	set info [string range $info $c1 end]
	set start [$w index $start+${c1}c]
    }
    ## Next stage, check for procs that start a line
    set start 1.0
    set exp(cmd) "^\"\[^\" \t\n\]+"
    while {
	[string compare {} [set ix \
		[$w search -regexp -count numc -- $exp(cmd) $start end]]]
    } {
	set start [$w index $ix+${numc}c]
	# +1c to avoid the first quote
	set cmd [$w get $ix+1c $start]
	if {[string match *::* $cmd]} {
	    set res [uplevel 1 ::tkcon::EvalOther $app namespace eval \
		    [list [namespace qualifiers $cmd] \
		    [list info procs [namespace tail $cmd]]]]
	} else {
	    set res [uplevel 1 ::tkcon::EvalOther $app info procs [list $cmd]]
	}
	if {[llength $res]==1} {
	    set tag [UniqueTag $w]
	    $w tag add $tag $ix+1c $start
	    $w tag configure $tag -foreground $COLOR(proc)
	    $w tag bind $tag <Enter> [list $w tag configure $tag -under 1]
	    $w tag bind $tag <Leave> [list $w tag configure $tag -under 0]
	    $w tag bind $tag <ButtonRelease-1> "if {!\$tk::Priv(mouseMoved)} \
		    {[list $OPT(edit) -attach $app -type proc -- $cmd]}"
	}
    }
}

# term_exit is called if the spawned process exits
proc ::tkcon::term_exit {w} {
    variable EXP
    catch {exp_close -i $EXP(spawn_id)}
    set EXP(forever) 1
    unset EXP
}

# term_chars_changed is called after every change to the displayed chars
# You can use if you want matches to occur in the background (a la bind)
# If you want to test synchronously, then just do so - you don't need to
# redefine this procedure.
proc ::tkcon::term_chars_changed {w args} {
}

# term_cursor_changed is called after the cursor is moved
proc ::tkcon::term_cursor_changed {w args} {
}

proc ::tkcon::term_update_cursor {w args} {
    variable OPT
    variable EXP

    $w mark set insert $EXP(row).$EXP(col)
    $w see insert
    term_cursor_changed $w
}

proc ::tkcon::term_clear {w args} {
    $w delete 1.0 end
    term_init $w
}

proc ::tkcon::term_init {w args} {
    variable OPT
    variable EXP

    # initialize it with blanks to make insertions later more easily
    set blankline [string repeat " " $OPT(cols)]\n
    for {set i 1} {$i <= $OPT(rows)} {incr i} {
	$w insert $i.0 $blankline
    }

    set EXP(row) 1
    set EXP(col) 0

    $w mark set insert $EXP(row).$EXP(col)
}

proc ::tkcon::term_down {w args} {
    variable OPT
    variable EXP

    if {$EXP(row) < $OPT(rows)} {
	incr EXP(row)
    } else {
	# already at last line of term, so scroll screen up
	$w delete 1.0 2.0

	# recreate line at end
	$w insert end [string repeat " " $OPT(cols)]\n
    }
}

proc ::tkcon::term_insert {w s} {
    variable OPT
    variable EXP

    set chars_rem_to_write [string length $s]
    set space_rem_on_line  [expr {$OPT(cols) - $EXP(col)}]

    set tag_action [expr {$EXP(standout) ? "add" : "remove"}]

    ##################
    # write first line
    ##################

    if {$chars_rem_to_write > $space_rem_on_line} {
	set chars_to_write $space_rem_on_line
	set newline 1
    } else {
	set chars_to_write $chars_rem_to_write
	set newline 0
    }

    $w delete $EXP(row).$EXP(col) \
	$EXP(row).[expr {$EXP(col) + $chars_to_write}]
    $w insert $EXP(row).$EXP(col) \
	[string range $s 0 [expr {$space_rem_on_line-1}]]

    $w tag $tag_action standout $EXP(row).$EXP(col) \
	$EXP(row).[expr {$EXP(col) + $chars_to_write}]

    # discard first line already written
    incr chars_rem_to_write -$chars_to_write
    set s [string range $s $chars_to_write end]

    # update EXP(col)
    incr EXP(col) $chars_to_write
    # update EXP(row)
    if {$newline} { term_down $w }

    ##################
    # write full lines
    ##################
    while {$chars_rem_to_write >= $OPT(cols)} {
	$w delete $EXP(row).0 $EXP(row).end
	$w insert $EXP(row).0 [string range $s 0 [expr {$OPT(cols)-1}]]
	$w tag $tag_action standout $EXP(row).0 $EXP(row).end

	# discard line from buffer
	set s [string range $s $OPT(cols) end]
	incr chars_rem_to_write -$OPT(cols)

	set EXP(col) 0
	term_down $w
    }

    #################
    # write last line
    #################

    if {$chars_rem_to_write} {
	$w delete $EXP(row).0 $EXP(row).$chars_rem_to_write
	$w insert $EXP(row).0 $s
	$w tag $tag_action standout $EXP(row).0 $EXP(row).$chars_rem_to_write
	set EXP(col) $chars_rem_to_write
    }

    term_chars_changed $w
}

## tkcon - command that allows control over the console
## This always exists in the main interpreter, and is aliased into
## other connected interpreters
# ARGS:	totally variable, see internal comments
## 
proc tkcon {cmd args} {
    variable ::tkcon::PRIV
    variable ::tkcon::OPT
    global errorInfo

    switch -glob -- $cmd {
	buf* {
	    ## 'buffer' Sets/Query the buffer size
	    if {[llength $args]} {
		if {[regexp {^[1-9][0-9]*$} $args]} {
		    set OPT(buffer) $args
		    # catch in case the console doesn't exist yet
		    catch {::tkcon::ConstrainBuffer $PRIV(console) \
			    $OPT(buffer)}
		} else {
		    return -code error "buffer must be a valid integer"
		}
	    }
	    return $OPT(buffer)
	}
	linelen* {
	    ## 'linelength' Sets/Query the maximum line length
	    if {[llength $args]} {
		if {[regexp {^-?[0-9]+$} $args]} {
		    set OPT(maxlinelen) $args
		} else {
		    return -code error "buffer must be a valid integer"
		}
	    }
	    return $OPT(maxlinelen)
	}
	bg* {
	    ## 'bgerror' Brings up an error dialog
	    set errorInfo [lindex $args 1]
	    bgerror [lindex $args 0]
	}
	cl* {
	    ## 'close' Closes the console
	    ::tkcon::Destroy
	}
	cons* {
	    ## 'console' - passes the args to the text widget of the console.
	    set result [uplevel 1 $PRIV(console) $args]
	    ::tkcon::ConstrainBuffer $PRIV(console) $OPT(buffer)
	    return $result
	}
	congets {
	    ## 'congets' a replacement for [gets stdin]
	    # Use the 'gets' alias of 'tkcon_gets' command instead of
	    # calling the *get* methods directly for best compatability
	    if {[llength $args]} {
		return -code error "wrong # args: must be \"tkcon congets\""
	    }
	    tkcon show
	    set old [bind TkConsole <<TkCon_Eval>>]
	    bind TkConsole <<TkCon_Eval>> { set ::tkcon::PRIV(wait) 0 }
	    set w $PRIV(console)
	    # Make sure to move the limit to get the right data
	    $w mark set limit end-1c
	    $w mark gravity limit left
	    $w mark set insert end
	    $w see end
	    vwait ::tkcon::PRIV(wait)
	    set line [::tkcon::CmdGet $w]
	    $w insert end \n
	    bind TkConsole <<TkCon_Eval>> $old
	    return $line
	}
	exp* {
	    ::tkcon::Expect [lindex $args 0]
	}
	getc* {
	    ## 'getcommand' a replacement for [gets stdin]
	    ## This forces a complete command to be input though
	    if {[llength $args]} {
		return -code error "wrong # args: must be \"tkcon getcommand\""
	    }
	    tkcon show
	    set old [bind TkConsole <<TkCon_Eval>>]
	    bind TkConsole <<TkCon_Eval>> { set ::tkcon::PRIV(wait) 0 }
	    set w $PRIV(console)
	    # Make sure to move the limit to get the right data
	    $w mark set insert end
	    $w mark set limit insert
	    $w see end
	    vwait ::tkcon::PRIV(wait)
	    set line [::tkcon::CmdGet $w]
	    $w insert end \n
	    while {![info complete $line] || [regexp {[^\\]\\$} $line]} {
		vwait ::tkcon::PRIV(wait)
		set line [::tkcon::CmdGet $w]
		$w insert end \n
		$w see end
	    }
	    bind TkConsole <<TkCon_Eval>> $old
	    return $line
	}
	get - gets {
	    ## 'gets' - a replacement for [gets stdin]
	    ## This pops up a text widget to be used for stdin (local grabbed)
	    if {[llength $args]} {
		return -code error "wrong # args: should be \"tkcon gets\""
	    }
	    set t $PRIV(base).gets
	    if {![winfo exists $t]} {
		toplevel $t
		wm withdraw $t
		catch {wm attributes $t -type dialog}
		wm title $t "tkcon gets stdin request"
		label $t.gets -text "\"gets stdin\" request:"
		text $t.data -width 32 -height 5 -wrap none \
			-xscrollcommand [list $t.sx set] \
			-yscrollcommand [list $t.sy set] -borderwidth 1
		scrollbar $t.sx -orient h -takefocus 0 -highlightthickness 0 \
			-command [list $t.data xview]
		scrollbar $t.sy -orient v -takefocus 0 -highlightthickness 0 \
			-command [list $t.data yview]
		button $t.ok -text "OK" -command {set ::tkcon::PRIV(grab) 1}
		bind $t.ok <Return> { %W invoke }
		grid $t.gets -		-sticky ew
		grid $t.data $t.sy	-sticky news
		grid $t.sx		-sticky ew
		grid $t.ok   -		-sticky ew
		grid columnconfig $t 0 -weight 1
		grid rowconfig    $t 1 -weight 1
		wm transient $t $PRIV(root)
		wm geometry $t +[expr {([winfo screenwidth $t]-[winfo \
			reqwidth $t]) / 2}]+[expr {([winfo \
			screenheight $t]-[winfo reqheight $t]) / 2}]
	    }
	    $t.data delete 1.0 end
	    wm deiconify $t
	    raise $t
	    grab $t
	    focus $t.data
	    vwait ::tkcon::PRIV(grab)
	    grab release $t
	    wm withdraw $t
	    return [$t.data get 1.0 end-1c]
	}
	err* {
	    ## Outputs stack caused by last error.
	    ## error handling with pizazz (but with pizza would be nice too)
	    if {[llength $args]==2} {
		set app  [lindex $args 0]
		set type [lindex $args 1]
		if {[catch {::tkcon::EvalOther $app $type set errorInfo} info]} {
		    set info "error getting info from $type $app:\n$info"
		}
	    } else {
		set info $PRIV(errorInfo)
	    }
	    if {[string match {} $info]} { set info "errorInfo empty" }
	    ## If args is empty, the -attach switch just ignores it
	    $OPT(edit) -attach $args -type error -- $info
	}
	fi* {
	    ## 'find' string
	    ::fndrpl::OldTkconFind $PRIV(console) $args
	}
	fo* {
	    ## 'font' ?fontname? - gets/sets the font of the console
	    if {[llength $args]} {
		if {[info exists PRIV(console)] && \
			[winfo exists $PRIV(console)]} {
		    $PRIV(console) config -font $args
		    set OPT(font) [$PRIV(console) cget -font]
		} else {
		    set OPT(font) $args
		}
	    }
	    return $OPT(font)
	}
	hid* - with* {
	    ## 'hide' 'withdraw' - hides the console.
	    if {[info exists PRIV(root)] && [winfo exists $PRIV(root)]} {
		wm withdraw $PRIV(root)
	    }
	}
	his* {
	    ## 'history'
	    set sub {\2}
	    if {[string match -new* $args]} { append sub "\n"}
	    set h [::tkcon::EvalSlave history]
	    regsub -all "( *\[0-9\]+  |\t)(\[^\n\]*\n?)" $h $sub h
	    return $h
	}
	ico* {
	    ## 'iconify' - iconifies the console with 'iconify'.
	    if {[info exists PRIV(root)] && [winfo exists $PRIV(root)]} {
		wm iconify $PRIV(root)
	    }
	}
	mas* - eval {
	    ## 'master' - evals contents in master interpreter
	    uplevel \#0 $args
	}
	result* {
	    ## 'resultfilter' Sets/queries the result filter command
	    if {[llength $args]} {
		set OPT(resultfilter) $args
	    }
	    return $OPT(resultfilter)
	}
	set {
	    ## 'set' - set (or get, or unset) simple vars (not whole arrays)
	    ## from the master console interpreter
	    ## possible formats:
	    ##    tkcon set <var>
	    ##    tkcon set <var> <value>
	    ##    tkcon set <var> <interp> <var1> <var2> w
	    ##    tkcon set <var> <interp> <var1> <var2> u
	    ##    tkcon set <var> <interp> <var1> <var2> r
	    if {[llength $args]==5} {
		## This is for use w/ 'tkcon upvar' and only works with slaves
		foreach {var i var1 var2 op} $args break
		if {[string compare {} $var2]} { append var1 "($var2)" }
		switch $op {
		    u { uplevel \#0 [list unset $var] }
		    w {
			return [uplevel \#0 [list set $var \
				[interp eval $i [list set $var1]]]]
		    }
		    r {
			return [interp eval $i [list set $var1 \
				[uplevel \#0 [list set $var]]]]
		    }
		}
	    } elseif {[llength $args] == 1} {
		upvar \#0 [lindex $args 0] var
		if {[array exists var]} {
		    return [array get var]
		} else {
		    return $var
		}
	    }
	    return [uplevel \#0 set $args]
	}
	append {
	    ## Modify a var in the master environment using append
	    return [uplevel \#0 append $args]
	}
	lappend {
	    ## Modify a var in the master environment using lappend
	    return [uplevel \#0 lappend $args]
	}
	show - deiconify {
	    ## 'show|deiconify' - deiconifies the console.
	    if {![info exists PRIV(root)]} {
	        error "tkcon embedded functionality removed. If you need embedded configuration, patch clcon"
	    }
	    if {![winfo exists $PRIV(root)]} {
		eval [linsert $args 0 ::tkcon::Init]
	    }
	    # this may throw an error if toplevel is embedded
	    catch {wm deiconify $PRIV(root); raise $PRIV(root)}
	    focus -force $PRIV(console)
	}
	ti* {
	    ## 'title' ?title? - gets/sets the console's title
	    if {[llength $args]} {
		return [wm title $PRIV(root) [join $args]]
	    } else {
		return [wm title $PRIV(root)]
	    }
	}
	upv* {
	    ## 'upvar' masterVar slaveVar
	    ## link slave variable slaveVar to the master variable masterVar
	    ## only works masters<->slave
	    set masterVar [lindex $args 0]
	    set slaveVar  [lindex $args 1]
	    if {[info exists $masterVar]} {
		interp eval $OPT(exec) \
			[list set $slaveVar [set $masterVar]]
	    } else {
		catch {interp eval $OPT(exec) [list unset $slaveVar]}
	    }
	    interp eval $OPT(exec) \
		    [list trace variable $slaveVar rwu \
		    [list tkcon set $masterVar $OPT(exec)]]
	    return
	}
	v* {
	    return $PRIV(version)
	}
	default {
	    ## tries to determine if the command exists, otherwise throws error
	    set new ::tkcon::[string toupper \
		    [string index $cmd 0]][string range $cmd 1 end]
	    if {[llength [info command $new]]} {
		uplevel \#0 $new $args
	    } else {
		return -code error "bad option \"$cmd\": must be\
			[join [lsort [list attach close console destroy \
			font hide iconify load main master new save show \
			slave deiconify version title bgerror]] {, }]"
	    }
	}
    }
}

##
## Some procedures to make up for lack of built-in shell commands
##


## tkcon_puts -
## This allows me to capture all stdout/stderr to the console window
## This will be renamed to 'puts' at the appropriate time during init
##
# ARGS:	same as usual	
# Outputs:	the string with a color-coded text tag
## 
proc tkcon_puts args {
    set len [llength $args]
    foreach {arg1 arg2 arg3} $args { break }

    if {$len == 1} {
	tkcon console insert output "$arg1\n" stdout
    } elseif {$len == 2} {
	if {![string compare $arg1 -nonewline]} {
	    tkcon console insert output $arg2 stdout
	} elseif {![string compare $arg1 stdout] \
		|| ![string compare $arg1 stderr]} {
	    tkcon console insert output "$arg2\n" $arg1
	} else {
	    set len 0
	}
    } elseif {$len == 3} {
	if {![string compare $arg1 -nonewline] \
		&& (![string compare $arg2 stdout] \
		|| ![string compare $arg2 stderr])} {
	    tkcon console insert output $arg3 $arg2
	} elseif {(![string compare $arg1 stdout] \
		|| ![string compare $arg1 stderr]) \
		&& ![string compare $arg3 nonewline]} {
	    tkcon console insert output $arg2 $arg1
	} else {
	    set len 0
	}
    } else {
	set len 0
    }

    ## $len == 0 means it wasn't handled by tkcon above.
    ##
    if {$len == 0} {
	global errorCode errorInfo
	if {[catch "tkcon_tcl_puts $args" msg]} {
	    regsub tkcon_tcl_puts $msg puts msg
	    regsub -all tkcon_tcl_puts $errorInfo puts errorInfo
	    return -code error $msg
	}
	return $msg
    }

    ## WARNING: This update should behave well because it uses idletasks,
    ## however, if there are weird looping problems with events, or
    ## hanging in waits, try commenting this out.
    if {$len} {
	tkcon console see output
	update idletasks
    }
}

## tkcon_gets -
## This allows me to capture all stdin input without needing to stdin
## This will be renamed to 'gets' at the appropriate time during init
##
# ARGS:		same as gets	
# Outputs:	same as gets
##
proc tkcon_gets args {
    set len [llength $args]
    if {$len != 1 && $len != 2} {
	return -code error \
		"wrong # args: should be \"gets channelId ?varName?\""
    }
    if {[string compare stdin [lindex $args 0]]} {
	return [uplevel 1 tkcon_tcl_gets $args]
    }
    set gtype [tkcon set ::tkcon::OPT(gets)]
    if {$gtype == ""} { set gtype congets }
    set data [tkcon $gtype]
    if {$len == 2} {
	upvar 1 [lindex $args 1] var
	set var $data
	return [string length $data]
    }
    return $data
}

proc ::tkcon::ReloadSomeIDESources2 {} {
    TkconSourceHere eval.tkcon.tcl
    TkconSourceHere edt_structure.edt.tcl
    TkconSourceHere editor_buffer.edt.tcl
    TkconSourceHere menu.recent.tcl
    TkconSourceHere edt_menu.edt.tcl
    TkconSourceHere editor.edt.tcl
    TkconSourceHere highlight.edt.tcl
    TkconSourceHere inspector.insp.tcl
    TkconSourceHere save.edt.tcl
    TkconSourceHere completions_menu.tcl
}

::tkcon::ReloadSomeIDESources2 

interp alias {} ::more {} ::edit
interp alias {} ::less {} ::edit

## echo
## Relaxes the one string restriction of 'puts'
# ARGS:	any number of strings to output to stdout
##
proc echo args { puts stdout [concat $args] }

## clear - clears the buffer of the console (not the history though)
## This is executed in the parent interpreter
## 
proc clear {{pcnt 100}} {
    if {![regexp {^[0-9]*$} $pcnt] || $pcnt < 1 || $pcnt > 100} {
	return -code error \
		"invalid percentage to clear: must be 1-100 (100 default)"
    } elseif {$pcnt == 100} {
	tkcon console delete 1.0 end
    } else {
	set tmp [expr {$pcnt/100.0*[tkcon console index end]}]
	tkcon console delete 1.0 "$tmp linestart"
    }
}

## alias - akin to the csh alias command
## If called with no args, then it dumps out all current aliases
## If called with one arg, returns the alias of that arg (or {} if none)
# ARGS:	newcmd	- (optional) command to bind alias to
# 	args	- command and args being aliased
## 
proc alias {{newcmd {}} args} {
    if {[string match {} $newcmd]} {
	set res {}
	foreach a [interp aliases] {
	    lappend res [list $a -> [interp alias {} $a]]
	}
	return [join $res \n]
    } elseif {![llength $args]} {
	interp alias {} $newcmd
    } else {
	eval interp alias [list {} $newcmd {}] $args
    }
}

## unalias - unaliases an alias'ed command
# ARGS:	cmd	- command to unbind as an alias
## 
proc unalias {cmd} {
    interp alias {} $cmd {}
}

## dump - outputs variables/procedure/widget info in source'able form.
## Accepts glob style pattern matching for the names
#
# ARGS:	type	- type of thing to dump: must be variable, procedure, widget
#
# OPTS: -nocomplain
#		don't complain if no items of the specified type are found
#	-filter pattern
#		specifies a glob filter pattern to be used by the variable
#		method as an array filter pattern (it filters down for
#		nested elements) and in the widget method as a config
#		option filter pattern
#	--	forcibly ends options recognition
#
# Returns:	the values of the requested items in a 'source'able form
## 
proc dump {type args} {
    set whine 1
    set code  ok
    if {![llength $args]} {
	## If no args, assume they gave us something to dump and
	## we'll try anything
	set args $type
	set type any
    }
    while {[string match -* [lindex $args 0]]} {
	switch -glob -- [lindex $args 0] {
	    -n* { set whine 0; set args [lreplace $args 0 0] }
	    -f* { set fltr [lindex $args 1]; set args [lreplace $args 0 1] }
	    --  { set args [lreplace $args 0 0]; break }
	    default {return -code error "unknown option \"[lindex $args 0]\""}
	}
    }
    if {$whine && ![llength $args]} {
	return -code error "wrong \# args: [lindex [info level 0] 0] type\
		?-nocomplain? ?-filter pattern? ?--? pattern ?pattern ...?"
    }
    set res {}
    switch -glob -- $type {
	c* {
	    # command
	    # outputs commands by figuring out, as well as possible, what it is
	    # this does not attempt to auto-load anything
	    foreach arg $args {
		if {[llength [set cmds [info commands $arg]]]} {
		    foreach cmd [lsort $cmds] {
			if {[lsearch -exact [interp aliases] $cmd] > -1} {
			    append res "\#\# ALIAS:   $cmd =>\
				    [interp alias {} $cmd]\n"
			} elseif {
			    [llength [info procs $cmd]] ||
			    ([string match *::* $cmd] &&
			    [llength [namespace eval [namespace qual $cmd] \
				    info procs [namespace tail $cmd]]])
			} {
			    if {[catch {dump p -- $cmd} msg] && $whine} {
				set code error
			    }
			    append res $msg\n
			} else {
			    append res "\#\# COMMAND: $cmd\n"
			}
		    }
		} elseif {$whine} {
		    append res "\#\# No known command $arg\n"
		    set code error
		}
	    }
	}
	v* {
	    # variable
	    # outputs variables value(s), whether array or simple.
	    if {![info exists fltr]} { set fltr * }
	    foreach arg $args {
		if {![llength [set vars [uplevel 1 info vars [list $arg]]]]} {
		    if {[uplevel 1 info exists $arg]} {
			set vars $arg
		    } elseif {$whine} {
			append res "\#\# No known variable $arg\n"
			set code error
			continue
		    } else { continue }
		}
		foreach var [lsort $vars] {
		    if {[uplevel 1 [list info locals $var]] == ""} {
			# use the proper scope of the var, but namespace which
			# won't id locals or some upvar'ed vars correctly
			set new [uplevel 1 \
				[list namespace which -variable $var]]
			if {$new != ""} {
			    set var $new
			}
		    }
		    upvar 1 $var v
		    if {[array exists v] || [catch {string length $v}]} {
			set nst {}
			append res "array set [list $var] \{\n"
			if {[array size v]} {
			    foreach i \
				    [lsort -dictionary [array names v $fltr]] {
				upvar 0 v\($i\) __a
				if {[array exists __a]} {
				    append nst "\#\# NESTED ARRAY ELEM: $i\n"
				    append nst "upvar 0 [list $var\($i\)] __a;\
					    [dump v -filter $fltr __a]\n"
				} else {
				    append res "    [list $i]\t[list $v($i)]\n"
				}
			    }
			} else {
			    ## empty array
			    append res "    empty array\n"
			    if {$var == ""} {
				append nst "unset (empty)\n"
			    } else {
				append nst "unset [list $var](empty)\n"
			    }
			}
			append res "\}\n$nst"
		    } else {
			append res [list set $var $v]\n
		    }
		}
	    }
	}
	p* {
	    # procedure
	    foreach arg $args {
		if {
		    ![llength [set procs [info proc $arg]]] &&
		    ([string match *::* $arg] &&
		    [llength [set ps [namespace eval \
			    [namespace qualifier $arg] \
			    info procs [namespace tail $arg]]]])
		} {
		    set procs {}
		    set namesp [namespace qualifier $arg]
		    foreach p $ps {
			lappend procs ${namesp}::$p
		    }
		}
		if {[llength $procs]} {
		    foreach p [lsort $procs] {
			set as {}
			foreach a [info args $p] {
			    if {[info default $p $a tmp]} {
				lappend as [list $a $tmp]
			    } else {
				lappend as $a
			    }
			}
			append res [list proc $p $as [info body $p]]\n
		    }
		} elseif {$whine} {
		    append res "\#\# No known proc $arg\n"
		    set code error
		}
	    }
	}
	w* {
	    # widget
	    ## The user should have Tk loaded
	    if {![llength [info command winfo]]} {
		return -code error "winfo not present, cannot dump widgets"
	    }
	    if {![info exists fltr]} { set fltr .* }
	    foreach arg $args {
		if {[llength [set ws [info command $arg]]]} {
		    foreach w [lsort $ws] {
			if {[winfo exists $w]} {
			    if {[catch {$w configure} cfg]} {
				append res "\#\# Widget $w\
					does not support configure method"
				set code error
			    } else {
				append res "\#\# [winfo class $w]\
					$w\n$w configure"
				foreach c $cfg {
				    if {[llength $c] != 5} continue
				    ## Check to see that the option does
				    ## not match the default, then check
				    ## the item against the user filter
				    if {[string compare [lindex $c 3] \
					    [lindex $c 4]] && \
					    [regexp -nocase -- $fltr $c]} {
					append res " \\\n\t[list [lindex $c 0]\
						[lindex $c 4]]"
				    }
				}
				append res \n
			    }
			}
		    }
		} elseif {$whine} {
		    append res "\#\# No known widget $arg\n"
		    set code error
		}
	    }
	}
	a* {
	    ## see if we recognize it, other complain
	    if {[regexp {(var|com|proc|widget)} \
		    [set types [uplevel 1 what $args]]]} {
		foreach type $types {
		    if {[regexp {(var|com|proc|widget)} $type]} {
			append res "[uplevel 1 dump $type $args]\n"
		    }
		}
	    } else {
		set res "dump was unable to resolve type for \"$args\""
		set code error
	    }
	}
	default {
	    return -code error "bad [lindex [info level 0] 0] option\
		    \"$type\": must be variable, command, procedure,\
		    or widget"
	}
    }
    return -code $code [string trimright $res \n]
}

## idebug - interactive debugger
#
# idebug body ?level?
#
#	Prints out the body of the command (if it is a procedure) at the
#	specified level.  <i>level</i> defaults to the current level.
#
# idebug break
#
#	Creates a breakpoint within a procedure.  This will only trigger
#	if idebug is on and the id matches the pattern.  If so, TkCon will
#	pop to the front with the prompt changed to an idebug prompt.  You
#	are given the basic ability to observe the call stack an query/set
#	variables or execute Tcl commands at any level.  A separate history
#	is maintained in debugging mode.
#
# idebug echo|{echo ?id?} ?args?
#
#	Behaves just like "echo", but only triggers when idebug is on.
#	You can specify an optional id to further restrict triggering.
#	If no id is specified, it defaults to the name of the command
#	in which the call was made.
#
# idebug id ?id?
#
#	Query or set the idebug id.  This id is used by other idebug
#	methods to determine if they should trigger or not.  The idebug
#	id can be a glob pattern and defaults to *.
#
# idebug off
#
#	Turns idebug off.
#
# idebug on ?id?
#
#	Turns idebug on.  If 'id' is specified, it sets the id to it.
#
# idebug puts|{puts ?id?} args
#
#	Behaves just like "puts", but only triggers when idebug is on.
#	You can specify an optional id to further restrict triggering.
#	If no id is specified, it defaults to the name of the command
#	in which the call was made.
#
# idebug show type ?level? ?VERBOSE?
#
#	'type' must be one of vars, locals or globals.  This method
#	will output the variables/locals/globals present in a particular
#	level.  If VERBOSE is added, then it actually 'dump's out the
#	values as well.  'level' defaults to the level in which this
#	method was called.
#
# idebug trace ?level?
#
#	Prints out the stack trace from the specified level up to the top
#	level.  'level' defaults to the current level.
#
##
proc idebug {opt args} {
    global IDEBUG

    if {![info exists IDEBUG(on)]} {
	array set IDEBUG { on 0 id * debugging 0 }
    }
    set level [expr {[info level]-1}]
    switch -glob -- $opt {
	on	{
	    if {[llength $args]} { set IDEBUG(id) $args }
	    return [set IDEBUG(on) 1]
	}
	off	{ return [set IDEBUG(on) 0] }
	id  {
	    if {![llength $args]} {
		return $IDEBUG(id)
	    } else { return [set IDEBUG(id) $args] }
	}
	break {
	    if {!$IDEBUG(on) || $IDEBUG(debugging) || \
		    ([llength $args] && \
		    ![string match $IDEBUG(id) $args]) || [info level]<1} {
		return
	    }
	    set IDEBUG(debugging) 1
	    puts stderr "idebug at level \#$level: [lindex [info level -1] 0]"
	    set tkcon [llength [info command tkcon]]
	    if {$tkcon} {
		tkcon master eval set ::tkcon::OPT(prompt2) \$::tkcon::OPT(prompt1)
		tkcon master eval set ::tkcon::OPT(prompt1) \$::tkcon::OPT(debugPrompt)
		set slave [tkcon set ::tkcon::OPT(exec)]
		set event [tkcon set ::tkcon::PRIV(event)]
		tkcon set ::tkcon::OPT(exec) [tkcon master interp create debugger]
		tkcon set ::tkcon::PRIV(event) 1
	    }
	    set max $level
	    while 1 {
		set err {}
		if {$tkcon} {
		    # tkcon's overload of gets is advanced enough to not need
		    # this, but we get a little better control this way.
		    tkcon evalSlave set level $level
		    tkcon prompt
		    set line [tkcon getcommand]
		    tkcon console mark set output end
		} else {
		    puts -nonewline stderr "(level \#$level) debug > "
		    gets stdin line
		    while {![info complete $line]} {
			puts -nonewline "> "
			append line "\n[gets stdin]"
		    }
		}
		if {[string match {} $line]} continue
		set key [regexp -inline {\S+} $line]
		if {![regexp {^\s*\S+\s+([#-]?[0-9]+)} $line -> lvl]} {
		    set lvl \#$level
		}
		set res {}; set c 0
		switch -- $key {
		    + {
			## Allow for jumping multiple levels
			if {$level < $max} {
			    idebug trace [incr level] $level 0 VERBOSE
			}
		    }
		    - {
			## Allow for jumping multiple levels
			if {$level > 1} {
			    idebug trace [incr level -1] $level 0 VERBOSE
			}
		    }
		    . { set c [catch {idebug trace $level $level 0 VERBOSE} res] }
		    v { set c [catch {idebug show vars $lvl } res] }
		    V { set c [catch {idebug show vars $lvl VERBOSE} res] }
		    l { set c [catch {idebug show locals $lvl } res] }
		    L { set c [catch {idebug show locals $lvl VERBOSE} res] }
		    g { set c [catch {idebug show globals $lvl } res] }
		    G { set c [catch {idebug show globals $lvl VERBOSE} res] }
		    t { set c [catch {idebug trace 1 $max $level } res] }
		    T { set c [catch {idebug trace 1 $max $level VERBOSE} res]}
		    b { set c [catch {idebug body $lvl} res] }
		    o { set res [set IDEBUG(on) [expr {!$IDEBUG(on)}]] }
		    h - ?	{
			puts stderr "    +		Move down in call stack
    -		Move up in call stack
    .		Show current proc name and params

    v		Show names of variables currently in scope
    V		Show names of variables currently in scope with values
    l		Show names of local (transient) variables
    L		Show names of local (transient) variables with values
    g		Show names of declared global variables
    G		Show names of declared global variables with values
    t		Show a stack trace
    T		Show a verbose stack trace

    b		Show body of current proc
    o		Toggle on/off any further debugging
    c,q		Continue regular execution (Quit debugger)
    h,?		Print this help
    default	Evaluate line at current level (\#$level)"
		    }
		    c - q break
		    default { set c [catch {uplevel \#$level $line} res] }
		}
		if {$tkcon} {
		    tkcon set ::tkcon::PRIV(event) \
			    [tkcon evalSlave eval history add [list $line]\
			    \; history nextid]
		}
		if {$c} {
		    puts stderr $res
		} elseif {[string compare {} $res]} {
		    puts $res
		}
	    }
	    set IDEBUG(debugging) 0
	    if {$tkcon} {
		tkcon master interp delete debugger
		tkcon master eval set ::tkcon::OPT(prompt1) \$::tkcon::OPT(prompt2)
		tkcon set ::tkcon::OPT(exec) $slave
		tkcon set ::tkcon::PRIV(event) $event
		tkcon prompt
	    }
	}
	bo* {
	    if {[regexp {^([#-]?[0-9]+)} $args level]} {
		return [uplevel $level {dump c -no [lindex [info level 0] 0]}]
	    }
	}
	t* {
	    if {[llength $args]<2} return
	    set min [set max [set lvl $level]]
	    set exp {^#?([0-9]+)? ?#?([0-9]+) ?#?([0-9]+)? ?(VERBOSE)?}
	    if {![regexp $exp $args junk min max lvl verbose]} return
	    for {set i $max} {
		$i>=$min && ![catch {uplevel \#$i info level 0} info]
	    } {incr i -1} {
		if {$i==$lvl} {
		    puts -nonewline stderr "* \#$i:\t"
		} else {
		    puts -nonewline stderr "  \#$i:\t"
		}
		set name [lindex $info 0]
		if {[string compare VERBOSE $verbose] || \
			![llength [info procs $name]]} {
		    puts $info
		} else {
		    puts "proc $name {[info args $name]} { ... }"
		    set idx 0
		    foreach arg [info args $name] {
			if {[string match args $arg]} {
			    puts "\t$arg = [lrange $info [incr idx] end]"
			    break
			} else {
			    puts "\t$arg = [lindex $info [incr idx]]"
			}
		    }
		}
	    }
	}
	s* {
	    #var, local, global
	    set level \#$level
	    if {![regexp {^([vgl][^ ]*) ?([#-]?[0-9]+)? ?(VERBOSE)?} \
		    $args junk type level verbose]} return
	    switch -glob -- $type {
		v* { set vars [uplevel $level {lsort [info vars]}] }
		l* { set vars [uplevel $level {lsort [info locals]}] }
		g* { set vars [lremove [uplevel $level {info vars}] \
			[uplevel $level {info locals}]] }
	    }
	    if {[string match VERBOSE $verbose]} {
		return [uplevel $level dump var -nocomplain $vars]
	    } else {
		return $vars
	    }
	}
	e* - pu* {
	    if {[llength $opt]==1 && [catch {lindex [info level -1] 0} id]} {
		set id [lindex [info level 0] 0]
	    } else {
		set id [lindex $opt 1]
	    }
	    if {$IDEBUG(on) && [string match $IDEBUG(id) $id]} {
		if {[string match e* $opt]} {
		    puts [concat $args]
		} else { eval puts $args }
	    }
	}
	default {
	    return -code error "bad [lindex [info level 0] 0] option \"$opt\",\
		    must be: [join [lsort [list on off id break print body\
		    trace show puts echo]] {, }]"
	}
    }
}

## observe - like trace, but not
# ARGS:	opt	- option
#	name	- name of variable or command
##
proc observe {opt name args} {
    global tcl_observe
    switch -glob -- $opt {
	co* {
	    if {[regexp {^(catch|lreplace|set|puts|for|incr|info|uplevel)$} \
		    $name]} {
		return -code error "cannot observe \"$name\":\
			infinite eval loop will occur"
	    }
	    set old ${name}@
	    while {[llength [info command $old]]} { append old @ }
	    rename $name $old
	    set max 4
	    regexp {^[0-9]+} $args max
	    # handle the observe'ing of 'proc'
	    set proccmd "proc"
	    if {[string match "proc" $name]} { set proccmd $old }
	    ## idebug trace could be used here
	    $proccmd $name args "
	    for {set i \[info level\]; set max \[expr \[info level\]-$max\]} {
		\$i>=\$max && !\[catch {uplevel \#\$i info level 0} info\]
	    } {incr i -1} {
		puts -nonewline stderr \"  \#\$i:\t\"
		puts \$info
	    }
	    uplevel \[lreplace \[info level 0\] 0 0 $old\]
	    "
	    set tcl_observe($name) $old
	}
	cd* {
	    if {[info exists tcl_observe($name)] && [catch {
		rename $name {}
		rename $tcl_observe($name) $name
		unset tcl_observe($name)
	    } err]} { return -code error $err }
	}
	ci* {
	    ## What a useless method...
	    if {[info exists tcl_observe($name)]} {
		set i $tcl_observe($name)
		set res "\"$name\" observes true command \"$i\""
		while {[info exists tcl_observe($i)]} {
		    append res "\n\"$name\" observes true command \"$i\""
		    set i $tcl_observe($name)
		}
		return $res
	    }
	}
	va* - vd* {
	    set type [lindex $args 0]
	    set args [lrange $args 1 end]
	    if {![regexp {^[rwu]} $type type]} {
		return -code error "bad [lindex [info level 0] 0] $opt type\
			\"$type\", must be: read, write or unset"
	    }
	    if {![llength $args]} { set args observe_var }
	    foreach c [uplevel 1 [list trace vinfo $name]] {
		# don't double up on the traces
		if {[list $type $args] == $c} { return }
	    }
	    uplevel 1 [list trace $opt $name $type $args]
	}
	vi* {
	    uplevel 1 [list trace vinfo $name]
	}
	default {
	    return -code error "bad [lindex [info level 0] 0] option\
		    \"[lindex $args 0]\", must be: [join [lsort \
		    [list command cdelete cinfo variable vdelete vinfo]] {, }]"
	}
    }
}

## observe_var - auxilary function for observing vars, called by trace
## via observe
# ARGS:	name	- variable name
#	el	- array element name, if any
#	op	- operation type (rwu)
##
proc observe_var {name el op} {
    if {[string match u $op]} {
	if {[string compare {} $el]} {
	    puts "unset \"${name}($el)\""
	} else {
	    puts "unset \"$name\""
	}
    } else {
	upvar 1 $name $name
	if {[info exists ${name}($el)]} {
	    puts [dump v ${name}($el)]
	} else {
	    puts [dump v $name]
	}
    }
}

## which - tells you where a command is found
# ARGS:	cmd	- command name
# Returns:	where command is found (internal / external / unknown)
## 
proc which cmd {
    ## This tries to auto-load a command if not recognized
    set types [uplevel 1 [list what $cmd 1]]
    if {[llength $types]} {
	set out {}
	
	foreach type $types {
	    switch -- $type {
		alias		{ set res "$cmd: aliased to [alias $cmd]" }
		procedure	{ set res "$cmd: procedure" }
		command		{ set res "$cmd: internal command" }
		executable	{ lappend out [auto_execok $cmd] }
		variable	{ lappend out "$cmd: $type" }
	    }
	    if {[info exists res]} {
		global auto_index
		if {[info exists auto_index($cmd)]} {
		    ## This tells you where the command MIGHT have come from -
		    ## not true if the command was redefined interactively or
		    ## existed before it had to be auto_loaded.  This is just
		    ## provided as a hint at where it MAY have come from
		    append res " ($auto_index($cmd))"
		}
		lappend out $res
		unset res
	    }
	}
	return [join $out \n]
    } else {
	return -code error "$cmd: command not found"
    }
}

## what - tells you what a string is recognized as
# ARGS:	str	- string to id
# Returns:	id types of command as list
## 
proc what {str {autoload 0}} {
    set types {}
    if {[llength [info commands $str]] || ($autoload && \
	    [auto_load $str] && [llength [info commands $str]])} {
	if {[lsearch -exact [interp aliases] $str] > -1} {
	    lappend types "alias"
	} elseif {
	    [llength [info procs $str]] ||
	    ([string match *::* $str] &&
	    [llength [namespace eval [namespace qualifier $str] \
		    info procs [namespace tail $str]]])
	} {
	    lappend types "procedure"
	} else {
	    lappend types "command"
	}
    }
    if {[llength [uplevel 1 info vars $str]]} {
	upvar 1 $str var
	if {[array exists var]} {
	    lappend types array variable
	} else {
	    lappend types scalar variable
	}
    }
    if {[file isdirectory $str]} {
	lappend types "directory"
    }
    if {[file isfile $str]} {
	lappend types "file"
    }
    if {[llength [info commands winfo]] && [winfo exists $str]} {
	lappend types "widget"
    }
    if {[string compare {} [auto_execok $str]]} {
	lappend types "executable"
    }
    return $types
}

## dir - directory list
# ARGS:	args	- names/glob patterns of directories to list
# OPTS:	-all	- list hidden files as well (Unix dot files)
#	-long	- list in full format "permissions size date filename"
#	-full	- displays / after directories and link paths for links
# Returns:	a directory listing
## 
proc dir {args} {
    array set s {
	all 0 full 0 long 0
	0 --- 1 --x 2 -w- 3 -wx 4 r-- 5 r-x 6 rw- 7 rwx
    }
    while {[string match \-* [lindex $args 0]]} {
	set str [lindex $args 0]
	set args [lreplace $args 0 0]
	switch -glob -- $str {
	    -a* {set s(all) 1} -f* {set s(full) 1}
	    -l* {set s(long) 1} -- break
	    default {
		return -code error "unknown option \"$str\",\
			should be one of: -all, -full, -long"
	    }
	}
    }
    set sep [string trim [file join . .] .]
    if {![llength $args]} { set args [list [pwd]] }
    if {$::tcl_version >= 8.3} {
	# Newer glob args allow safer dir processing.  The user may still
	# want glob chars, but really only for file matching.
	foreach arg $args {
	    if {[file isdirectory $arg]} {
		if {$s(all)} {
		    lappend out [list $arg [lsort \
			    [glob -nocomplain -directory $arg .* *]]]
		} else {
		    lappend out [list $arg [lsort \
			    [glob -nocomplain -directory $arg *]]]
		}
	    } else {
		set dir [file dirname $arg]
		lappend out [list $dir$sep [lsort \
			[glob -nocomplain -directory $dir [file tail $arg]]]]
	    }
	}
    } else {
	foreach arg $args {
	    if {[file isdirectory $arg]} {
		set arg [string trimright $arg $sep]$sep
		if {$s(all)} {
		    lappend out [list $arg [lsort [glob -nocomplain -- $arg.* $arg*]]]
		} else {
		    lappend out [list $arg [lsort [glob -nocomplain -- $arg*]]]
		}
	    } else {
		lappend out [list [file dirname $arg]$sep \
			[lsort [glob -nocomplain -- $arg]]]
	    }
	}
    }
    if {$s(long)} {
	set old [clock scan {1 year ago}]
	set fmt "%s%9ld %s %s\n"
	foreach o $out {
	    set d [lindex $o 0]
	    append res $d:\n
	    foreach f [lindex $o 1] {
		file lstat $f st
		set f [file tail $f]
		if {$s(full)} {
		    switch -glob $st(type) {
			d* { append f $sep }
			l* { append f "@ -> [file readlink $d$sep$f]" }
			default { if {[file exec $d$sep$f]} { append f * } }
		    }
		}
		if {[string match file $st(type)]} {
		    set mode -
		} else {
		    set mode [string index $st(type) 0]
		}
		foreach j [split [format %03o [expr {$st(mode)&0777}]] {}] {
		    append mode $s($j)
		}
		if {$st(mtime)>$old} {
		    set cfmt {%b %d %H:%M}
		} else {
		    set cfmt {%b %d  %Y}
		}
		append res [format $fmt $mode $st(size) \
			[clock format $st(mtime) -format $cfmt] $f]
	    }
	    append res \n
	}
    } else {
	foreach o $out {
	    set d [lindex $o 0]
	    append res "$d:\n"
	    set i 0
	    foreach f [lindex $o 1] {
		if {[string len [file tail $f]] > $i} {
		    set i [string len [file tail $f]]
		}
	    }
	    set i [expr {$i+2+$s(full)}]
	    set j 80
	    ## This gets the number of cols in the tkcon console widget
	    if {[llength [info commands tkcon]]} {
		set j [expr {[tkcon master set ::tkcon::OPT(cols)]/$i}]
	    }
	    set k 0
	    foreach f [lindex $o 1] {
		set f [file tail $f]
		if {$s(full)} {
		    switch -glob [file type $d$sep$f] {
			d* { append f $sep }
			l* { append f @ }
			default { if {[file exec $d$sep$f]} { append f * } }
		    }
		}
		append res [format "%-${i}s" $f]
		if {$j == 0 || [incr k]%$j == 0} {
		    set res [string trimright $res]\n
		}
	    }
	    append res \n\n
	}
    }
    return [string trimright $res]
}
interp alias {} ::ls {} ::dir -full

## lremove - remove items from a list
# OPTS:
#   -all	remove all instances of each item
#   -glob	remove all instances matching glob pattern
#   -regexp	remove all instances matching regexp pattern
# ARGS:	l	a list to remove items from
#	args	items to remove (these are 'join'ed together)
##
proc lremove {args} {
    array set opts {-all 0 pattern -exact}
    while {[string match -* [lindex $args 0]]} {
	switch -glob -- [lindex $args 0] {
	    -a*	{ set opts(-all) 1 }
	    -g*	{ set opts(pattern) -glob }
	    -r*	{ set opts(pattern) -regexp }
	    --	{ set args [lreplace $args 0 0]; break }
	    default {return -code error "unknown option \"[lindex $args 0]\""}
	}
	set args [lreplace $args 0 0]
    }
    set l [lindex $args 0]
    foreach i [join [lreplace $args 0 0]] {
	if {[set ix [lsearch $opts(pattern) $l $i]] == -1} continue
	set l [lreplace $l $ix $ix]
	if {$opts(-all)} {
	    while {[set ix [lsearch $opts(pattern) $l $i]] != -1} {
		set l [lreplace $l $ix $ix]
	    }
	}
    }
    return $l
}

proc ::tkcon::ReloadSomeIDESources3 {} {
    if { $::tkcon::ENABLE_UNKNOWN == 1 } {
        TkconSourceHere tkcon-unknown.tcl
    } elseif { $::tkcon::ENABLE_UNKNOWN == -1 } {
        TkconSourceHere tkcon-unknown-mini.tcl
    }
    TkconSourceHere bindings.tkcon.tcl
}

::tkcon::ReloadSomeIDESources3

##
# ::tkcon::PopupMenu - what to do when the popup menu is requested
##
proc ::tkcon::PopupMenu {X Y} {
    variable PRIV
    variable OPT

    set w $PRIV(console)
    if {[string compare $w [winfo containing $X $Y]]} {
	tk_popup $PRIV(popup) $X $Y
	return
    }
    set x [expr {$X-[winfo rootx $w]}]
    set y [expr {$Y-[winfo rooty $w]}]
    if {[llength [set tags [$w tag names @$x,$y]]]} {
	if {[lsearch -exact $tags "proc"] >= 0} {
	    lappend type "proc"
	    foreach {first last} [$w tag prevrange proc @$x,$y] {
		set word [$w get $first $last]; break
	    }
	}
	if {[lsearch -exact $tags "var"] >= 0} {
	    lappend type "var"
	    foreach {first last} [$w tag prevrange var @$x,$y] {
		set word [$w get $first $last]; break
	    }
	}
    }
    if {![info exists type]} {
	set exp "(^|\[^\\\\\]\[ \t\n\r\])"
	set exp2 "\[\[\\\\\\?\\*\]"
	set i [$w search -backwards -regexp $exp @$x,$y "@$x,$y linestart"]
	if {[string compare {} $i]} {
	    if {![string match *.0 $i]} {append i +2c}
	    if {[string compare {} \
		    [set j [$w search -regexp $exp $i "$i lineend"]]]} {
		append j +1c
	    } else {
		set j "$i lineend"
	    }
	    regsub -all $exp2 [$w get $i $j] {\\\0} word
	    set word [string trim $word {\"$[]{}',?#*}]
	    if {[llength [EvalAttached [list info commands $word]]]} {
		lappend type "proc"
	    }
	    if {[llength [EvalAttached [list info vars $word]]]} {
		lappend type "var"
	    }
	    if {[EvalAttached [list file isfile $word]]} {
		lappend type "file"
	    }
	}
    }
    if {![info exists type] || ![info exists word]} {
	tk_popup $PRIV(popup) $X $Y
	return
    }
    $PRIV(context) delete 0 end
    $PRIV(context) add command -label "$word" -state disabled
    $PRIV(context) add separator
    set app [Attach]
    if {[lsearch $type proc] != -1} {
	$PRIV(context) add command -label "View Procedure" \
		-command [list $OPT(edit) -attach $app -type proc -- $word]
    }
    if {[lsearch $type var] != -1} {
	$PRIV(context) add command -label "View Variable" \
		-command [list $OPT(edit) -attach $app -type var -- $word]
    }
    if {[lsearch $type file] != -1} {
	$PRIV(context) add command -label "View File" \
		-command [list $OPT(edit) -attach $app -type file -- $word]
    }
    tk_popup $PRIV(context) $X $Y
}

## ::tkcon::TagProc - tags a procedure in the console if it's recognized
## This procedure is not perfect.  However, making it perfect wastes
## too much CPU time...
##
proc ::tkcon::TagProc w {
    # budden: here we extract current identifier and ask attached interpreter what does the word mean
    # in lisp, we would run modified side-effect-free reader instead. Now we disable the machinery as
    # this is not of key importance to get working system. slime.el passes autodoc events in similar circumstances,
    # e.g. thus it receives prompt of function argument list
    
    
    #budden set exp "\[^\\\\\]\[\[ \t\n\r\;{}\"\$\]"
    #budden set i [$w search -backwards -regexp $exp insert-1c limit-1c]
    #budden if {[string compare {} $i]} {append i +2c} else {set i limit}
    #budden regsub -all "\[\[\\\\\\?\\*\]" [$w get $i "insert-1c wordend"] {\\\0} c
    #budden if {[llength [EvalAttached [list info commands $c]]]} {
	#budden $w tag add proc $i "insert-1c wordend"
    #budden } else {
	#budden $w tag remove proc $i "insert-1c wordend"
    #budden }
    #budden if {[llength [EvalAttached [list info vars $c]]]} {
	#budden $w tag add var $i "insert-1c wordend"
    #budden } else {
	#budden $w tag remove var $i "insert-1c wordend"
    #budden }
}

## ::tkcon::MatchPair - blinks a matching pair of characters
## c2 is assumed to be at the text index 'insert'.
## This proc is really loopy and took me an hour to figure out given
## all possible combinations with escaping except for escaped \'s.
## It doesn't take into account possible commenting... Oh well.  If
## anyone has something better, I'd like to see/use it.  This is really
## only efficient for small contexts.
# ARGS:	w	- console text widget
# 	c1	- first char of pair
# 	c2	- second char of pair
# Calls:	::tkcon::Blink
## 
proc ::tkcon::MatchPair {w c1 c2 {lim 1.0}} {
    if {[string compare {} [set ix [$w search -back $c1 insert $lim]]]} {
	while {
	    [string match {\\} [$w get $ix-1c]] &&
	    [string compare {} [set ix [$w search -back $c1 $ix-1c $lim]]]
	} {}
	set i1 insert-1c
	while {[string compare {} $ix]} {
	    set i0 $ix
	    set j 0
	    while {[string compare {} [set i0 [$w search $c2 $i0 $i1]]]} {
		append i0 +1c
		if {[string match {\\} [$w get $i0-2c]]} continue
		incr j
	    }
	    if {!$j} break
	    set i1 $ix
	    while {$j && [string compare {} \
		    [set ix [$w search -back $c1 $ix $lim]]]} {
		if {[string match {\\} [$w get $ix-1c]]} continue
		incr j -1
	    }
	}
	if {[string match {} $ix]} { set ix [$w index $lim] }
    } else { set ix [$w index $lim] }
    if {$::tkcon::OPT(blinkrange)} {
	Blink $w $ix [$w index insert]
    } else {
	Blink $w $ix $ix+1c [$w index insert-1c] [$w index insert]
    }
}

## ::tkcon::MatchQuote - blinks between matching quotes.
## Blinks just the quote if it's unmatched, otherwise blinks quoted string
## The quote to match is assumed to be at the text index 'insert'.
# ARGS:	w	- console text widget
# Calls:	::tkcon::Blink
## 
proc ::tkcon::MatchQuote {w {lim 1.0}} {
    set i insert-1c
    set j 0
    while {[string compare [set i [$w search -back \" $i $lim]] {}]} {
	if {[string match {\\} [$w get $i-1c]]} continue
	if {!$j} {set i0 $i}
	incr j
    }
    if {$j&1} {
	if {$::tkcon::OPT(blinkrange)} {
	    Blink $w $i0 [$w index insert]
	} else {
	    Blink $w $i0 $i0+1c [$w index insert-1c] [$w index insert]
	}
    } else {
	Blink $w [$w index insert-1c] [$w index insert]
    }
}

## ::tkcon::Blink - blinks between n index pairs for a specified duration.
# ARGS:	w	- console text widget
# 	i1	- start index to blink region
# 	i2	- end index of blink region
# 	dur	- duration in usecs to blink for
# Outputs:	blinks selected characters in $w
## 
proc ::tkcon::Blink {w args} {
    eval [list $w tag add blink] $args
    after $::tkcon::OPT(blinktime) [list $w] tag remove blink $args
    return
}


## ::tkcon::Insert
## Insert a string into a text console at the point of the insertion cursor.
## If there is a selection in the text, and it covers the point of the
## insertion cursor, then delete the selection before inserting.
# ARGS:	w	- text window in which to insert the string
# 	s	- string to insert (usually just a single char)
# Outputs:	$s to text widget
## 
proc ::tkcon::Insert {w s} {
    if {[string match {} $s] || [string match disabled [$w cget -state]]} {
	return
    }
    variable EXP
    if {[info exists EXP(spawn_id)]} {
	exp_send -i $EXP(spawn_id) -- $s
	return
    }
    if {[$w comp insert < limit]} {
	$w mark set insert end
    }
    if {[llength [$w tag ranges sel]] && \
	    [$w comp sel.first <= insert] && [$w comp sel.last >= insert]} {
	$w delete sel.first sel.last
    }
    $w insert insert $s
    $w see insert
}


# Here is a group of functions that is only used when Tkcon is
# executed in a safe interpreter. It provides safe versions of
# missing functions. For example:
#
# - "tk appname" returns "tkcon.tcl" but cannot be set
# - "toplevel" is equivalent to 'frame', only it is automatically
#   packed.
# - The 'source', 'load', 'open', 'file' and 'exit' functions are
#   mapped to corresponding functions in the parent interpreter.
#
# Further on, Tk cannot be really loaded. Still the safe 'load'
# provedes a speciall case. The Tk can be divided into 4 groups,
# that each has a safe handling procedure.
#
# - "::tkcon::SafeItem" handles commands like 'button', 'canvas' ......
#   Each of these functions has the window name as first argument.
# - "::tkcon::SafeManage" handles commands like 'pack', 'place', 'grid',
#   'winfo', which can have multiple window names as arguments.
# - "::tkcon::SafeWindow" handles all windows, such as '.'. For every
#   window created, a new alias is formed which also is handled by
#   this function.
# - Other (e.g. bind, bindtag, image), which need their own function.
#
## These functions courtesy Jan Nijtmans
##
if {![llength [info commands tk]]} {
    proc tk {option args} {
	if {![string match app* $option]} {
	    error "wrong option \"$option\": should be appname"
	}
	return "tkcon.tcl"
    }
}

if {![llength [info command toplevel]]} {
    proc toplevel {name args} {
	eval [linsert $args 0 frame $name]
	grid $name -sticky news
    }
}

proc ::tkcon::SafeSource {i f} {
    set fd [open $f r]
    set r [read $fd]
    close $fd
    if {[catch {interp eval $i $r} msg]} {
	error $msg
    }
}

proc ::tkcon::SafeOpen {i f {m r}} {
    set fd [open $f $m]
    interp transfer {} $fd $i
    return $fd
}

proc ::tkcon::SafeLoad {i f p} {
    global tk_version tk_patchLevel tk_library auto_path
    if {[string compare $p Tk]} {
	load $f $p $i
    } else {
	foreach command {button canvas checkbutton entry frame label
	listbox message radiobutton scale scrollbar spinbox text toplevel} {
	    $i alias $command ::tkcon::SafeItem $i $command
	}
	$i alias image ::tkcon::SafeImage $i
	foreach command {pack place grid destroy winfo} {
	    $i alias $command ::tkcon::SafeManage $i $command
	}
	if {[llength [info command event]]} {
	    $i alias event ::tkcon::SafeManage $i $command
	}
	frame .${i}_dot -width 300 -height 300 -relief raised
	pack .${i}_dot -side left
	$i alias tk tk
	$i alias bind ::tkcon::SafeBind $i
	$i alias bindtags ::tkcon::SafeBindtags $i
	$i alias . ::tkcon::SafeWindow $i {}
	foreach var {tk_version tk_patchLevel tk_library auto_path} {
	    $i eval [list set $var [set $var]]
	}
	$i eval {
	    package provide Tk $tk_version
	    if {[lsearch -exact $auto_path $tk_library] < 0} {
		lappend auto_path $tk_library
	    }
	}
	return ""
    }
}

proc ::tkcon::SafeSubst {i a} {
    set arg1 ""
    foreach {arg value} $a {
	if {![string compare $arg -textvariable] ||
	![string compare $arg -variable]} {
	    set newvalue "[list $i] $value"
	    global $newvalue
	    if {[interp eval $i info exists $value]} {
		set $newvalue [interp eval $i set $value]
	    } else {
		catch {unset $newvalue}
	    }
	    $i eval trace variable $value rwu \{[list tkcon set $newvalue $i]\}
	    set value $newvalue
	} elseif {![string compare $arg -command]} {
	    set value [list $i eval $value]
	}
	lappend arg1 $arg $value
    }
    return $arg1
}

proc ::tkcon::SafeItem {i command w args} {
    set args [::tkcon::SafeSubst $i $args]
    set code [catch "$command [list .${i}_dot$w] $args" msg]
    $i alias $w ::tkcon::SafeWindow $i $w
    regsub -all .${i}_dot $msg {} msg
    return -code $code $msg
}

proc ::tkcon::SafeManage {i command args} {
    set args1 ""
    foreach arg $args {
	if {[string match . $arg]} {
	    set arg .${i}_dot
	} elseif {[string match .* $arg]} {
	    set arg ".${i}_dot$arg"
	}
	lappend args1 $arg
    }
    set code [catch "$command $args1" msg]
    regsub -all .${i}_dot $msg {} msg
    return -code $code $msg
}

#
# FIX: this function doesn't work yet if the binding starts with '+'.
#
proc ::tkcon::SafeBind {i w args} {
    if {[string match . $w]} {
	set w .${i}_dot
    } elseif {[string match .* $w]} {
	set w ".${i}_dot$w"
    }
    if {[llength $args] > 1} {
	set args [list [lindex $args 0] \
		"[list $i] eval [list [lindex $args 1]]"]
    }
    set code [catch "bind $w $args" msg]
    if {[llength $args] <2 && $code == 0} {
	set msg [lindex $msg 3]
    }
    return -code $code $msg
}

proc ::tkcon::SafeImage {i option args} {
    set code [catch "image $option $args" msg]
    if {[string match cr* $option]} {
	$i alias $msg $msg
    }
    return -code $code $msg
}

proc ::tkcon::SafeBindtags {i w {tags {}}} {
    if {[string match . $w]} {
	set w .${i}_dot
    } elseif {[string match .* $w]} {
	set w ".${i}_dot$w"
    }
    set newtags {}
    foreach tag $tags {
	if {[string match . $tag]} {
	    lappend newtags .${i}_dot
	} elseif {[string match .* $tag]} {
	    lappend newtags ".${i}_dot$tag"
	} else {
	    lappend newtags $tag
	}
    }
    if {[string match $tags {}]} {
	set code [catch {bindtags $w} msg]
	regsub -all \\.${i}_dot $msg {} msg
    } else {
	set code [catch {bindtags $w $newtags} msg]
    }
    return -code $code $msg
}

proc ::tkcon::SafeWindow {i w option args} {
    if {[string match conf* $option] && [llength $args] > 1} {
	set args [::tkcon::SafeSubst $i $args]
    } elseif {[string match itemco* $option] && [llength $args] > 2} {
	set args "[list [lindex $args 0]] [::tkcon::SafeSubst $i [lrange $args 1 end]]"
    } elseif {[string match cr* $option]} {
	if {[llength $args]%2} {
	    set args "[list [lindex $args 0]] [::tkcon::SafeSubst $i [lrange $args 1 end]]"
	} else {
	    set args [::tkcon::SafeSubst $i $args]
	}
    } elseif {[string match bi* $option] && [llength $args] > 2} {
	set args [list [lindex $args 0] [lindex $args 1] "[list $i] eval [list [lindex $args 2]]"]
    }
    set code [catch ".${i}_dot$w $option $args" msg]
    if {$code} {
	regsub -all .${i}_dot $msg {} msg
    } elseif {[string match conf* $option] || [string match itemco* $option]} {
	if {[llength $args] == 1} {
	    switch -- $args {
		-textvariable - -variable {
		    set msg "[lrange $msg 0 3] [list [lrange [lindex $msg 4] 1 end]]"
		}
		-command - updatecommand {
		    set msg "[lrange $msg 0 3] [list [lindex [lindex $msg 4] 2]]"
		}
	    }
	} elseif {[llength $args] == 0} {
	    set args1 ""
	    foreach el $msg {
		switch -- [lindex $el 0] {
		    -textvariable - -variable {
			set el "[lrange $el 0 3] [list [lrange [lindex $el 4] 1 end]]"
		    }
		    -command - updatecommand {
			set el "[lrange $el 0 3] [list [lindex [lindex $el 4] 2]]"
		    }
		}
		lappend args1 $el
	    }
	    set msg $args1
	}
    } elseif {[string match cg* $option] || [string match itemcg* $option]} {
	switch -- $args {
	    -textvariable - -variable {
		set msg [lrange $msg 1 end]
	    }
	    -command - updatecommand {
		set msg [lindex $msg 2]
	    }
	}
    } elseif {[string match bi* $option]} {
	if {[llength $args] == 2 && $code == 0} {
	    set msg [lindex $msg 2]
	}
    }
    return -code $code $msg
}

## 'send' package that handles multiple communication variants
##
# Try using Tk send first, then look for a winsend interp,
# then try dde and finally have a go at comm
namespace eval ::send {}
proc ::send::send {args} {
    set winfoInterpCmd [list ::winfo interps]
    array set opts [list displayof {} async 0]
    while {[string match -* [lindex $args 0]]} {
	switch -exact -- [lindex $args 0] {
	    -displayof {
		set opts(displayof) [Pop args 1]
		lappend winfoInterpCmd -displayof $opts(displayof)
	    }
	    -async     { set opts(async) 1 }
	    -- { Pop args ; break }
	    default {
		return -code error "bad option \"[lindex $args 0]\":\
		    should be -displayof, -async or --"
	    }
	}
	Pop args
    }
    set app [Pop args]

    if {[llength [info commands ::winfo]]
	&& [lsearch -exact [eval $winfoInterpCmd] $app] > -1} {
	set cmd [list ::send]
	if {$opts(async) == 1} {lappend cmd -async}
	if {$opts(displayof) != {}} {lappend cmd -displayof $opts(displayof)}
	lappend cmd $app
	eval $cmd $args
    } elseif {[llength [info commands ::winsend]]
	      && [lsearch -exact [::winsend interps] $app] > -1} {
	eval [list ::winsend send $app] $args
    } elseif {[llength [info commands ::dde]]
	      && [lsearch -exact [dde services TclEval {}] \
		      [list TclEval $app]] > -1} {
	eval [list ::dde eval $app] $args
    } elseif {[package provide comm] != {}
	      && [regexp {^[0-9]+$} [lindex $app 0]]} {
	#if {$opts(displayof) != {} && [llength $app] == 1} {
	#    lappend app $opts(displayof)
	#}
	eval [list ::comm::comm send $app] $args
    } else {
	return -code error "bad interp: \"$app\" could not be found"
    }
}

proc ::send::interps {args} {
    set winfoInterpCmd [list ::winfo interps]
    array set opts [list displayof {}]
    while {[string match -* [lindex $args 0]]} {
	switch -exact -- [lindex $args 0] {
	    -displayof {
		set opts(displayof) [Pop args 1]
		lappend winfoInterpCmd -displayof $opts(displayof)
	    }
	    --	       { Pop args ; break }
	    default {
		return -code error "bad option \"[lindex $args 0]\":\
		    should be -displayof or --"
	    }
	}
	Pop args
    }

    set interps {}
    if {[llength [info commands ::winfo]]} {
	set interps [concat $interps [eval $winfoInterpCmd]]
    }
    if {[llength [info commands ::winsend]]} {
	set interps [concat $interps [::winsend interps]]
    }
    if {[llength [info commands ::dde]]} {
	set servers {}
	foreach server [::dde services TclEval {}] {
	    lappend servers [lindex $server 1]
	}
	set interps [concat $interps $servers]
    }
    if {[package provide comm] != {}} {
	set interps [concat $interps [::comm::comm interps]]
    }
    return $interps
}

proc ::send::appname {args} {
    set appname {}
    if {[llength [info commands ::tk]]} {
	set appname [eval ::tk appname $args]
    }
    if {[llength [info commands ::winsend]]} {
	set appname [concat $appname [eval ::winsend appname $args]]
    }
    if {[llength [info commands ::dde]]} {
	set appname [concat $appname [eval ::dde servername $args]]
    }
    # comm? can set port num and local/global interface.
    return [lsort -unique $appname]
}

proc ::send::Pop {varname {nth 0}} {
    upvar $varname args
    set r [lindex $args $nth]
    set args [lreplace $args $nth $nth]
    return $r
}
##
## end 'send' package

## special case 'tk appname' in Tcl plugin
if {$::tkcon::PRIV(WWW)} {
    rename tk ::tkcon::_tk
    proc tk {cmd args} {
	if {$cmd == "appname"} {
	    return "tkcon/WWW"
	} else {
	    return [uplevel 1 ::tkcon::_tk [list $cmd] $args]
	}
    }
}

## ::tkcon::Resource - re'source's this script into current console
## Meant primarily for my development of this program.  It follows
## links until the ultimate source is found.
##
proc ::tkcon::Resource {} {
    uplevel \#0 {
	if {[catch {source -rsrc tkcon}]} { source $::tkcon::PRIV(SCRIPT) }
    }
    Bindings
    InitSlave $::tkcon::OPT(exec)
}

#interp eval {} "proc ::CurIntPath {puts} { set result $slave \n if { \$puts ne {} } {             puts  \"CurIntPath = \$result\ (\$puts)\" } \n return \$result }"
interp eval {} "proc ::CurIntPath {puts} { }"

## Initialize only if we haven't yet, and do other stuff that prepares to
## run.  It only actually inits (and runs) tkcon if it is the main script.
##
proc ::tkcon::AtSource {} {
    variable PRIV

    puts stderr "Entering ::tkcon::AtSource"

    # the info script assumes we always call this while being sourced
    set PRIV(SCRIPT) [info script]
    if {!$PRIV(WWW) && [string length $PRIV(SCRIPT)]} {
	if {[info tclversion] >= 8.4} {
	    set PRIV(SCRIPT) [file normalize $PRIV(SCRIPT)]
	} else {
	    # we use a catch here because some wrap apps choke on 'file type'
	    # because TclpLstat wasn't wrappable until 8.4.
	    catch {
		while {[string match link [file type $PRIV(SCRIPT)]]} {
		    set link [file readlink $PRIV(SCRIPT)]
		    if {[string match relative [file pathtype $link]]} {
			set PRIV(SCRIPT) \
			    [file join [file dirname $PRIV(SCRIPT)] $link]
		    } else {
			set PRIV(SCRIPT) $link
		    }
		}
		catch {unset link}
		if {[string match relative [file pathtype $PRIV(SCRIPT)]]} {
		    set PRIV(SCRIPT) [file join [pwd] $PRIV(SCRIPT)]
		}
	    }
	}
    }
    # normalize argv0 if it was tkcon to ensure that we'll be able
    # to load slaves correctly.
    if {[info exists ::argv0] && [info script] == $::argv0} {
	set ::argv0 $PRIV(SCRIPT)
    }

    if {(![info exists PRIV(root)] || ![winfo exists $PRIV(root)]) \
	    && ([info exists ::argv0] && $PRIV(SCRIPT) == $::argv0)} {
	global argv
	if {[info exists argv]} {
	    eval ::tkcon::Init $argv
	} else {
	    ::tkcon::Init
	}
    }
}
tkcon::AtSource

package require tablelist

proc ::tkcon::ReloadSomeIDESources4 {} {                                    
    # it must work after toplevel window creation, or
    # unwanted toplevel windows arises 
    TkconSourceHere tablelist_util.tcl
    TkconSourceHere search-tablelist.srchtblst.tcl
    TkconSourceHere error-browser.erbr.tcl
    TkconSourceHere grep-browser.grbr.tcl
    TkconSourceHere buffer-list.buli.tcl
    TkconSourceHere lisp-debugger.ldbg.tcl
    TkconSourceHere tcltextedit-unsorted.tcl
    TkconSourceHere clconcmd_inner.tcl
    TkconSourceHere clconcmd.tcl
    TkconSourceHere clcon-odu-commands.odu.tcl
    TkconSourceHere expand.tkcon.tcl
}

::tkcon::ReloadSomeIDESources4

proc ::tkcon::ReloadSomeIDESources {} {
    ReloadSomeIDESources1
    ReloadSomeIDESources2
    ReloadSomeIDESources3
    ReloadSomeIDESources4
}

package provide tkcon $::tkcon::VERSION

::tkcon::OuterNewSwank
