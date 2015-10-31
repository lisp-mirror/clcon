## This file is a part of clcon. Eval and friends. 

namespace eval ::tkcon {

    ## ::tkcon::Eval - evaluates commands input into console window
    ## This is the first stage of the evaluating commands in the console.
    ## They need to be broken up into consituent commands (by ::tkcon::CmdSep) in
    ## case a multiple commands were pasted in, then each is eval'ed (by
    ## ::tkcon::EvalCmd) in turn.  Any uncompleted command will not be eval'ed.
    # ARGS:	w	- console text widget
    # Calls:	::tkcon::CmdGet, ::tkcon::CmdSep, ::tkcon::EvalCmd
    ## 
    proc Eval {w} {
        variable PRIV
        set gotcmd [CmdGet $w]
        if {$::swcnn::CurrentSwankConnection ne {}} {
            # FIXME: this is an async command, can we return from it? 
            EvalInSwankFromConsole $w
            return
        }
        
        set complete [CmdSep $gotcmd cmds last]
        $w mark set insert end-1c
        $w insert end \n
        if {[llength $cmds]} {
            foreach c $cmds {EvalCmd $w $c}
            $w insert insert $last {}
        } elseif {$complete} {
            EvalCmd $w $last
        }
        if {[winfo exists $w]} {
            $w see insert
        }
    }


    ## ::tkcon::EvalCmd - evaluates a single command, adding it to history
    # ARGS:	w	- console text widget
    # 	cmd	- the command to evaluate
    # Calls:	::tkcon::Prompt
    # Outputs:	result of command to stdout (or stderr if error occured)
    # Returns:	next event number
    ## 
    proc EvalCmd {w cmd} {
        variable OPT
        variable PRIV

        # tr "evalCmd $cmd"

        $w mark set output end
        if {$cmd ne ""} {
            set code 0
            
            if {$OPT(subhistory)} {
                set ev [EvalSlave history nextid]
                incr ev -1
                if {$cmd eq "!!"} {
                    set code [catch {EvalSlave history event $ev} cmd]
                    if {!$code} {$w insert output $cmd\n stdin}
                } elseif {[regexp {^!(.+)$} $cmd dummy event]} {
                    ## Check last event because history event is broken
                    set code [catch {EvalSlave history event $ev} cmd]
                    if {!$code && ![string match ${event}* $cmd]} {
                        set code [catch {EvalSlave history event $event} cmd]
                    }
                    if {!$code} {$w insert output $cmd\n stdin}
                } elseif {[regexp {^\^([^^]*)\^([^^]*)\^?$} $cmd dummy old new]} {
                    set code [catch {EvalSlave history event $ev} cmd]
                    if {!$code} {
                        regsub -all -- $old $cmd $new cmd
                        $w insert output $cmd\n stdin
                    }
                } 
            }
            if {$code} {
                $w insert output $cmd\n stderr
            } else {
                ## We are about to evaluate the command, so move the limit
                ## mark to ensure that further <Return>s don't cause double
                ## evaluation of this command - for cases like the command
                ## has a vwait or something in it
                $w mark set limit end
                if {$OPT(nontcl) && ($PRIV(apptype) eq "interp")} {
                    tr "About to enter EvalSend"
                    set code [catch {EvalSend $cmd} res]
                    if {$code == 1} {
                        set PRIV(errorInfo) "Non-Tcl errorInfo not available"
                    }
                } else {
                    set code [catch {EvalAttached $cmd} res]
                    if {$code == 1} {
                        if {[catch {EvalAttached [list set errorInfo]} err]} {
                            set PRIV(errorInfo) "Error getting errorInfo:\n$err"
                        } else {
                            set PRIV(errorInfo) $err
                        }
                    }
                }
                if {![winfo exists $w]} {
                    # early abort - must be a deleted tab
                    return
                }
                AddSlaveHistory $cmd
                # Run any user defined result filter command.  The command is
                # passed result code and data.
                if {[llength $OPT(resultfilter)]} {
                    set cmd [linsert $OPT(resultfilter) end $code $res]
                    if {[catch {EvalAttached $cmd} res2]} {
                        $w insert output "Filter failed: $res2" stderr \n stdout
                    } else {
                        set res $res2
                    }
                }
                #budden catch {EvalAttached [list set _ $res]}
                set maxlen $OPT(maxlinelen)
                set trailer ""
                if {($maxlen > 0) && ([string length $res] > $maxlen)} {
                    # If we exceed maximum desired output line length, truncate
                    # the result and add "...+${num}b" in error coloring
                    set trailer ...+[expr {[string length $res]-$maxlen}]b
                    set res [string range $res 0 $maxlen]
                }
                if {$code} {
                    if {$OPT(hoterrors)} {
                        set tag [UniqueTag $w]
                        $w insert output $res [list stderr $tag] \n$trailer stderr
                        $w tag bind $tag <Enter> \
			    [list $w tag configure $tag -under 1]
                        $w tag bind $tag <Leave> \
			    [list $w tag configure $tag -under 0]
                        $w tag bind $tag <ButtonRelease-1> \
			    "if {!\[info exists tk::Priv(mouseMoved)\] || !\$tk::Priv(mouseMoved)} \
			    {[list $OPT(edit) -attach [Attach] -type error -- $PRIV(errorInfo)]}"
                    } else {
                        $w insert output $res\n$trailer stderr
                    }
                } elseif {$res ne ""} {
                    $w insert output $res stdout $trailer stderr \n stdout
                }
            }
        }
        Prompt
        set PRIV(event) [EvalSlave history nextid]
    }

    ## ::tkcon::EvalSlave - evaluates the args in the associated slave
    ## args should be passed to this procedure like they would be at
    ## the command line (not like to 'eval').
    # ARGS:	args	- the command and args to evaluate
    ##
    proc EvalSlave args {
        interp eval $::tkcon::OPT(exec) $args
    }

    ## ::tkcon::EvalOther - evaluate a command in a foreign interp or slave
    ## without attaching to it.  No check for existence is made.
    # ARGS:	app	- interp/slave name
    #	type	- (slave|interp)
    ##
    proc EvalOther { app type args } {
        if {$type eq "slave"} {
            return [Slave $app $args]
        } else {
            return [uplevel 1 ::send::send [list $app] $args]
        }
    }

    ## ::tkcon::AddSlaveHistory - 
    ## Command is added to history only if different from previous command.
    ## This also doesn't cause the history id to be incremented, although the
    ## command will be evaluated.
    # ARGS: cmd	- command to add
    ##
    proc AddSlaveHistory cmd {
        set ev [EvalSlave history nextid]
        incr ev -1
        set code [catch {EvalSlave history event $ev} lastCmd]
        if {$code || $cmd ne $lastCmd} {
            EvalSlave history add $cmd
        }
    }

    ## ::tkcon::EvalSend - sends the args to the attached interpreter
    ## Varies from 'send' by determining whether attachment is dead
    ## when an error is received
    # ARGS:	cmd	- the command string to send across
    # Returns:	the result of the command
    ##
    proc EvalSend cmd {
        variable OPT
        variable PRIV

        puts stderr "::tkcon::EvalSend invoked :("
        if {$PRIV(deadapp)} {
            if {[lsearch -exact [::send::interps] $PRIV(app)]<0} {
                return
            } else {
                set PRIV(appname) [string range $PRIV(appname) 5 end]
                set PRIV(deadapp) 0
                Prompt "\n\"$PRIV(app)\" alive\n" [CmdGet $PRIV(console)]
            }
        }
        set code [catch {::send::send -displayof $PRIV(displayWin) $PRIV(app) $cmd} result]
        if {$code && [lsearch -exact [::send::interps] $PRIV(app)]<0} {
            ## Interpreter disappeared
            if {($OPT(dead) ne "leave") &&
                (($OPT(dead) eq "ignore") ||
                 [tk_messageBox -title "Dead Attachment" -type yesno \
                      -icon info -message \
                      "\"$PRIV(app)\" appears to have died.\
		\nReturn to primary slave interpreter?"] eq "no")} {
                set PRIV(appname) "DEAD:$PRIV(appname)"
                set PRIV(deadapp) 1
            } else {
                set err "Attached Tk interpreter \"$PRIV(app)\" died."
                Attach {}
                set PRIV(deadapp) 0
                EvalSlave set errorInfo $err
            }
            Prompt \n [CmdGet $PRIV(console)]
        }
        return -code $code $result
    }

    ## ::tkcon::EvalSocketEvent - fileevent command for an interpreter attached
    ## via a tcp/ip socket
    ## Must determine whether socket is dead when an error is received
    # ARGS:	args	- the args to send across
    # Returns:	the result of the command
    ##
    proc EvalSocketEvent {sock} {
        variable PRIV

        if {[gets $sock line] == -1} {
            if {[eof $sock]} {
                EvalSocketClosed $sock
            }
            return
        }
        puts $line
    }

    ## ::tkcon::EvalSocketClosed - takes care of handling a closed eval socket
    ##
    # ARGS:	args	- the args to send across
    # Returns:	the result of the command
    ##
    proc EvalSocketClosed {sock} {
        variable OPT
        variable PRIV

        putd "Entered EvalSocketClosed"

        upvar \#0 $::swcnn::CurrentSwankConnection con
        set OurChannel $con(sock)
        
        if {$sock eq $OurChannel} {
            # This is our connection
            puts stderr "Our SWANK connection is dead. Returning to tcl interpreter"
            DisconnectFromSwank
            # ::swcnn::TerminateCurrentConnection
        } else {
            puts stderr "Strange that some alien connection is closed"
            showVar sock
            showVar OurChannel
            catch {close $sock}
            return
        }
        
    }

    ## ::tkcon::EvalNamespace - evaluates the args in a particular namespace
    ## This is an override for ::tkcon::EvalAttached for when the user wants
    ## to attach to a particular namespace of the attached interp
    # ARGS:	attached	
    #	namespace	the namespace to evaluate in
    #	args		the args to evaluate
    # RETURNS:	the result of the command
    ##
    proc EvalNamespace { attached namespace args } {
        if {[llength $args]} {
            uplevel \#0 $attached \
		[list [concat [list namespace eval $namespace] $args]]
        }
    }


    ## ::tkcon::Namespaces - return all the namespaces descendent from $ns
    ##
    #
    ##
    proc Namespaces {{ns ::} {l {}}} {
        if {$ns ne ""} { lappend l $ns }
        foreach i [EvalAttached [list namespace children $ns]] {
            set l [Namespaces $i $l]
        }
        return $l
    }

    ## ::tkcon::CmdGet - gets the current command from the console widget
    # ARGS:	w	- console text widget
    # Returns:	text which compromises current command line
    ## 
    proc CmdGet w {
        if {![llength [$w tag nextrange prompt limit end]]} {
            $w tag add stdin limit end-1c
            return [$w get limit end-1c]
        }
    }

    ## ::tkcon::CmdSep - separates multiple commands into a list and remainder
    # ARGS:	cmd	- (possible) multiple command to separate
    # 	list	- varname for the list of commands that were separated.
    #	last	- varname of any remainder (like an incomplete final command).
    #		If there is only one command, it's placed in this var.
    # Returns:	constituent command info in varnames specified by list & rmd.
    ## 
    proc CmdSep {cmd list last} {
        upvar 1 $list cmds $last inc
        set inc {}
        set cmds {}
        foreach c [split [string trimleft $cmd] \n] {
            if {$inc ne ""} {
                append inc \n$c
            } else {
                append inc [string trimleft $c]
            }
            if {[info complete $inc] && ![regexp {[^\\]\\$} $inc]} {
                if {[regexp "^\[^#\]" $inc]} {lappend cmds $inc}
                set inc {}
            }
        }
        set i [string equal $inc {}]
        if {$i && $cmds ne "" && ![string match *\n $cmd]} {
            set inc [lindex $cmds end]
            set cmds [lreplace $cmds end end]
        }
        return $i
    }

    ## ::tkcon::CmdSplit - splits multiple commands into a list
    # ARGS:	cmd	- (possible) multiple command to separate
    # Returns:	constituent commands in a list
    ## 
    proc CmdSplit {cmd} {
        set inc {}
        set cmds {}
        foreach cmd [split [string trimleft $cmd] \n] {
            if {$inc ne ""} {
                append inc \n$cmd
            } else {
                append inc [string trimleft $cmd]
            }
            if {[info complete $inc] && ![regexp {[^\\]\\$} $inc]} {
                #set inc [string trimright $inc]
                if {[regexp "^\[^#\]" $inc]} {lappend cmds $inc}
                set inc {}
            }
        }
        if {[regexp "^\[^#\]" $inc]} {lappend cmds $inc}
        return $cmds
    }

    ## ::tkcon::ConstrainBuffer - This limits the amount of data in the text widget
    ## Called by ::tkcon::Prompt and in tkcon proc buffer/console switch cases
    # ARGS:	w	- console text widget
    #	size	- # approximate no of lines to constrain to
    # Outputs:	may delete data in console widget
    ## 
    proc ConstrainBuffer {w size} {
        set curend [expr {int([$w index end])}]
        if {$size && ($curend > $size)} {
            set newsize [expr {round(0.75*$size)}]
            set newstart [expr {max(1,$curend - $size)}]
            set newstartindex [string cat $newstart ".0"]
            $w delete 1.0 $newstart.0
        }
    }

    ## ::tkcon::Prompt - displays the prompt in the console widget
    # ARGS:	w	- console text widget
    # Outputs:	prompt (specified in ::tkcon::OPT(prompt1)) to console
    ## 
    proc Prompt {{pre {}} {post {}} {prompt {}}} {
        variable OPT
        variable PRIV
        global IDEBUG

        set w $PRIV(console)
        if {![winfo exists $w]} { return }
        # puts stderr [string cat < [$w get end-2c end-1c] >]
        catch {
            if { [$w get end-2c end-1c] ne "\n" } {
               $w insert end "\n"
            }
        }
        if {$pre ne ""} { $w insert end $pre stdout }
        set i [$w index end-1c]

        
        # budden: check for debugging is my code. This was not needed in tkcon,
        # but I've changed way tkcon work and things are bit out of
        # my control. idebug works, and I will not study why.
        # If it works, I just print an appropriate prompt

        if {[info exists IDEBUG]} {
            set debugging $IDEBUG(debugging)
        } else {
            set debugging 0
        }
        if {$::swcnn::CurrentSwankConnection ne {} && !$debugging } {
            $w insert end [join [list [EvalSlave history nextid] " " $PRIV(CurrentPackageDisplayName) ">"] ""] prompt
        } else {
            if {!$OPT(showstatusbar)} {
                if {$PRIV(appname) ne ""} {
                    $w insert end ">$PRIV(appname)< " prompt
                }
                if {$PRIV(namesp) ne "::"} {
                    $w insert end "<$PRIV(namesp)> " prompt
                }
            }
            if {$prompt ne ""} {
                $w insert end $prompt prompt
            } else {
                $w insert end [EvalSlave subst $OPT(prompt1)] prompt
            }
        }

        
        $w mark set output $i
        $w mark set insert end
        $w mark set limit insert
        $w mark gravity limit left
        if {$post ne ""} { $w insert end $post stdin }
        ConstrainBuffer $w $OPT(buffer)
        set ::tkcon::PRIV(StatusCursor) [$w index insert]
        $w see end
    }

    proc RePrompt {{pre {}} {post {}} {prompt {}}} {
        # same as prompt, but does nothing for those actions where we
        # only wanted to refresh the prompt on attach change when the
        # statusbar is showing (which carries that info instead)
        variable OPT
        if {!$OPT(showstatusbar)} {
            Prompt $pre $post $prompt
        }
    }

    # Parses return from swank-repl:create-repl
    # See also ChangeCurrentPackageB
    proc ChangeCurrentPackageA {EventAsList} {
        variable PRIV
        set x [::mprs::ParseReturnOk $EventAsList]
        set PRIV(CurrentPackageName) [::mprs::Unleash [lindex $x 0]]
        set PRIV(CurrentPackageDisplayName) [::mprs::Unleash [lindex $x 1]]
    }
       

    # See also ChangeCurrentPackageA
    # new-package event received from slime
    # second elt is package name, third is package nickname
    proc ChangeCurrentPackageB {EventAsList} {
        variable PRIV
        set PRIV(CurrentPackageName) [::mprs::Unleash [lindex $EventAsList 1]]
        set PRIV(CurrentPackageDisplayName) [::mprs::Unleash [lindex $EventAsList 2]]
        puts stderr "PRIV(CurrentPackageName) = $PRIV(CurrentPackageName)"
    }
}
