## Copyright (c) 1995-2011 Jeffrey Hobbs, jeff(a)hobbs(.)org
## Copyright (c) 2015 Denis Budyak
## MIT License

# Command evaluation with tcl escape handling and history substitution
# Main checkpoints are:
# ::tkcon::EvalInSwankAsync {form continuation {ItIsListenerEval 1} {ThreadDesignator {}} {ContinuationCounter {}}  - normal evaluation
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


proc ::clconcmd::history {} {
    tkcon main history
}

proc ::tkcon::TclEscapeP { cmd } {
    if {[string index $cmd 0] eq "."} {
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

# returns two values:
# 1. number of dots afore or "history" if it is a history reference
# 2. form cleared from dots
# Errors: errs if incorrect prefix
proc ::tkcon::ClassifyTclEscapes { form } {
    if {![TclEscapeP $form]} {
        return [list 0 $form]
    } elseif {[string range $form 0 3] eq "...."} {
        error "Unknown 'dotted' command $form"
    } elseif {[string range $form 0 2] eq "..."} {
        return [list 3 [string range $form 3 end]]
    } elseif {[string range $form 0 1] eq ".."} {
        return [list 2 [string range $form 2 end]]
    } else {
        # Hence we have one dot only
        set FormWoPrefix [string range $form 1 end]
        foreach {number NumLength} [scan $FormWoPrefix "%d%n"] {break}
        if {$NumLength == [string length $FormWoPrefix]} {
            # it is a history reference
            return [list "history" $FormWoPrefix]
        } else {
            return [list 1 $FormWoPrefix]
        }
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
    # ... - tkcon main
    # .. - just eval in main interpreter 
    # . - manage history or add ::clconcmd:: to resolve clcon command

    if {$TclEscapeKind == 3} {
        set RealForm [string cat "tkcon main {" $RealForm  "}"]
    } elseif {$TclEscapeKind == 2} {
        # ok
    } elseif {$TclEscapeKind == 1} {
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
                [list $w tag configure $tag -under 1]
            $w tag bind $tag <Leave> \
                [list $w tag configure $tag -under 0]
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
        } elseif { $TclEscapeKind ne 0 } {
            set res [EvalTclEscape $w $TclEscapeKind $RealForm $cmd]
            return $res
        } else {
            # FIXME. I suppose this is slow. How to use apply here? Budden
            set res [EvalInSwankAsync $RealForm \
                         "::tkcon::EvalInSwankFromConsoleContinuation $w \$EventAsList [list $RealForm]"]
            return $res
        }
    } result]

    # For lisp, end of processing is executed after returning from a command
    # see EvalInSwankFromConsoleContinuation

    if {$code == 1 || $TclEscapeKind ne 0 && $TclEscapeKind ne {history}} {
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

