## Copyright (c) 1995-2011 Jeffrey Hobbs, jeff(a)hobbs(.)org
## Copyright (c) 2015-2017 Denis Budyak
## MIT License

# Command evaluation with tcl escape handling and history substitution
# Main checkpoints are:
# ::tkcon::EvalInSwankAsync {form continuation {ThreadDesignator {}} {ContinuationCounter {}}  - normal evaluation
# where w and cmd are local variables, $EventAsList is a parameter of Continuation.
# continuation maybe of form
#"::tkcon::EvalInSwankFromConsoleContinuation $w \$EventAsList [list $cmd]"
# where $w and $cmd are local variables. $cmd is put into list to be passed as string/
# $EventAsList is preceded by \  not to be evaluated too early.
#
# ::tkcon::EvalInSwankSync - synchronously evaluate lisp in "t" swank thread and return result
# E.g. for completion 
#
# ::tkcon::EvalInSwankFromConsole - especially for evaluation of command typed in from the console
# Manages history, prompt, etc.
# Dont forget to qualify all symbols you use in your command
# 


proc ::tkcon::TclEscapeP { cmd } {
    if {[lsearch [list { } {.}] [string index $cmd 0]] >= 0} {
        return 1
    } else {
        return 0
    }
}

# Shows prompt, allocates new history id
proc ::tkcon::DoAfterCommand {} {
    variable PRIV
    Prompt
    set PRIV(event) [EvalSlave history nextid]
}

proc ::tkcon::ClassifyTclEscapes { form } {
    if {![TclEscapeP $form]} {
        return [list te0 $form]
    } elseif {[string range $form 0 0] eq " "} {
        return [list te3 [string range $form 1 end]]
    } elseif {[string range $form 0 2] eq "..."} {
        error "Unknown 'dotted' command $form"
    } elseif {[string range $form 0 1] eq ".."} {
        return [list te2 [string range $form 2 end]]
    } elseif {[string range $form 0 0] eq "."} {
        set FormWoPrefix [string range $form 1 end]
        foreach {number NumLength} [scan $FormWoPrefix "%d%n"] {break}
        if {$NumLength == [string length $FormWoPrefix]} {
            # it is a history reference
            set f  [string range $form 1 end]
            set f [list history $f]
            return $f
        } else {
            return [list te1 $FormWoPrefix]
        }
    } else {
        error {Ошибка в алгоритме}
    }
}

# Args: FormWoPrefix : form w/o dots, we know that it is a history substitution
# Returns: If this form is a number, returns history event from that number
# In case of error, returns code and error message
proc ::tkcon::ExpandFormFromHistory {FormWoPrefix form} {
    foreach {number length} [scan $FormWoPrefix "%d%n"] {break}
    if {$length != [string length $FormWoPrefix]} {
        error "Internal error 75474"
    }
    set result [EvalSlave history event $number]
   
    #Protect from recursion
    foreach {kind2 FormWoPrefix2} [ClassifyTclEscapes $result] {break}
    if {$kind2 eq {history}} {
        error "Recursive history reference in command $form"
    }
    return $result
}


# Can throw errors!
proc ::tkcon::EvalTclEscape { w TclEscapeKind RealForm form} {
    # Tcl Escape is classified already
    # tcl escape: if lisp command starts from . , we (temporarily?) consider it as tcl escape
    # .. - just eval in main interpreter 
    # . - manage history or add ::clconcmd:: to resolve clcon command

    if {[string compare $TclEscapeKind "te3"] == 0} {
        set RealForm [string cat "tkcon main {" $RealForm  "}"]
    } elseif {[string compare $TclEscapeKind "te2"] == 0} {
        # ok
    } elseif {[string compare $TclEscapeKind "te1"] == 0} {
        set RealForm [string cat "::clconcmd::" $RealForm]
    } else {
        error "Internal error 253525"
    }
    # I believe this code is called in main interpreter
    # It seems that PRIV and OPT are only available in main interpreter
    # puts "EvalAttached = [interp alias ::tkcon::EvalAttached]"
    
    # It looks like error management is done by caller
    set res [::tkcon::EvalAttached $RealForm]
    
    # this is lame but it works!
    # Correct code for working with results see in EvalAttached or something like this.
    return $res
}

# We either finished processing of command, or we get a error
# Process error and print a prompt so that user can enter next command
proc ::tkcon::EndProcessingOfNonLispCommandOrError {w cmd code res} {
    variable OPT
    variable PRIV

    if {$code == 1} {
        set PRIV(errorInfo) $::errorInfo
        # was - "Socket-based errorInfo not available", but command can fail on tcl side
    }
    if {![winfo exists $w]} {
        # early abort - must be a deleted tab
        return
    }

    if {$code == 1} {
        if {$OPT(hoterrors)} {
            set tag [UniqueTag $w]
            $w insert output $res [list stderr $tag] \n stdout
            $w tag bind $tag <Enter> \
                [list $w tag configure $tag -underline 1]
            $w tag bind $tag <Leave> \
                [list $w tag configure $tag -underline 0]
            $w tag bind $tag <ButtonRelease-1> \
                "if {!\[info exists tk::Priv(mouseMoved)\] || !\$tk::Priv(mouseMoved)} \
                {[list $OPT(edit) -attach [Attach] -type error -- $PRIV(errorInfo)]}"
        } else {
            $w insert output $res\n stderr
        }                
    } elseif {$res ne ""} {
        $w insert output $res stdout \n stdout
    }

    AddSlaveHistory $cmd
    DoAfterCommand
}


# Part of evaluation mechanism - we have command accepted from user.
# Code is taken from EvalCmd
# Errors: should not throw
proc ::tkcon::EvalKnownCommand { w cmd } {
    variable OPT
    variable PRIV

    if {$cmd eq ""} {
        return
    }

    set code 0
    
    ## We are about to evaluate the command, so move the limit
    ## mark to ensure that further <Return>s don't cause double
    ## evaluation of this command - for cases like the command
    ## has a vwait or something in it
    $w mark set limit end

    # we don't know what command we have
    set result [ClassifyTclEscapes $cmd]
    set cms [ClassifyTclEscapes $cmd]
    set code 0
    set cm [string range $cms 0 6]
    if {$cm eq "history"} {
     set code 1
    }
    if {$code} {  
     set cms [string range $cms 7 end]
     variable PRIV
     set w $PRIV(console)
     set nextid [EvalSlave history nextid]
     set a [expr   -$cms + $nextid]
     DoAfterCommand
     set i 0;
     while {0<[expr $a - $i]} {
       set res [::tkcon::Event -1]
       incr i; 		 
     }
     return $res
    } 

    set code [catch {ClassifyTclEscapes $cmd} ClassifiedCommand]  
    
    if {$code} {
        EndProcessingOfNonLispCommandOrError $w $cmd $code $ClassifiedCommand
        return {}
    }
    
    foreach {TclEscapeKind RealForm} $ClassifiedCommand {break}

    set code [catch {
        if {$TclEscapeKind eq {history}} {
            set ffh [ExpandFormFromHistory $RealForm $cmd]
            if {$ffh eq ""} {
                error "Empty command after history expansion of $form"
            }
            $w insert output $ffh\n stdin
            set res [EvalKnownCommand $w $ffh]
            return $res
        } elseif { $TclEscapeKind eq "te3" } {
            set res [SendEventToSwank $RealForm \
                         "::tkcon::EvalInSwankFromConsoleContinuation $w \$EventAsList [list $RealForm]" 1]
            return $res
        } elseif { $TclEscapeKind ne "te0" } {
            set res [EvalTclEscape $w $TclEscapeKind $RealForm $cmd]
            return $res
        } else {
            # FIXME. I suppose this is slow. How to use apply here?
            # tip: grep apply in the whole project
            set res [SendEventToSwank $RealForm \
                         "::tkcon::EvalInSwankFromConsoleContinuation $w \$EventAsList [list $RealForm]" 1]
            return $res
        }
    } result]

    # For lisp, end of processing is executed after returning from a command
    # see EvalInSwankFromConsoleContinuation

    if {$code == 1 || ([lsearch {te0 {history} te3} $TclEscapeKind] < 0)}  {
        EndProcessingOfNonLispCommandOrError $w $cmd $code $result
    }
    
    return $result
}


## Clone of ::tkcon::Eval (see clcon.tcl) and ::tkcon::EvalCmd
# Some code is lost while copying... 
proc ::tkcon::EvalInSwankFromConsole { w } {
    
    set cmd [CmdGet $w]
    # we do not split commands
    $w mark set insert end-1c
    $w insert end \n

    #Code from EvalCmd follows

    $w mark set output end

    return [EvalKnownCommand $w $cmd]
}



# procedure 
proc ::tkcon::EvalInSwankFromConsoleContinuation {w EventAsList cmd} {
    putd "EISFCC : EventAsList = $EventAsList"
    if {![winfo exists $w]} {
        # early abort - must be a deleted tab
        return
    }

    AddSlaveHistory $cmd

    set caadr [::mprs::Car [lindex $EventAsList 1]]
    if { $caadr eq {:ok} } {
        # normal return, do nothing
    } elseif { $caadr eq {:abort} } {
        set cadr [::mprs::Unleash [lindex $EventAsList 1]]
        set explanation [::mprs::Unleash [lindex $cadr 1]]
        puts stderr "Evaluation aborted at $explanation"
    }

    # No need to do output - it is done when :write-string was processed
    # puts "::tkcon::EvalInSwankFromConsoleContinuation Event = $Event "

    DoAfterCommand
}

