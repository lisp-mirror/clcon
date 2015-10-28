# ::clcon_text::clcon_text Widget based on example from snit FAQ
# NEVER USE PUTS or SHOWVAR in this file: it calls update so everything breaks
# Options:
#   -readonly
#   -send_to_lisp
# Subcommands
#   pathName Freeze
#   pathName Unfreeze
#
# If -readonly option == 1, this makes text widget readonly,
# keeping an ability to receive focus
# and a visible cursor for keyboard navigation.
# Readonly text
# text can still be altered with
# RoInsert, RoDelete, RoReplace
#
# If -readonly option == 0, widget can be operated normally.
# RoInsert, RoDelete, RoReplace subprocedures still work. (or how
#
#
# If -send_to_lisp options is set, all text modifications are
# sent to lisp. Do not change this option after editing startup.
#
# Freeze/unfreeze - utility for synchronous buffer modifications.
#
# pathName Freeze
#
#  - makes widget to collect all events it receives instead of processing them
# pathName Unfreeze
#
#  - makes event to process all delayed event.
# Denis Budyak 2015
# #include <MIT License>

package require Tk
package require snit
# package require ctext

# First of all, fix for the following problem:
# When user presses Shift-Enter and holds it for a long time, windows keyboard returns code 13 in a time
# This causes our world to crash (oduvanchik mark position mismatch. This is why we ignore this event and also inform user about the problem)
proc CheckIfKeyPressIsNotReturn {a} {
    set acode -
    scan $a %c acode
	if {$acode == 13} {
	    tk_messageBox -title "Anomaly" -message "Char 13 (return) generated by keyboard. Unable to handle it"
		error "Char 13 (return) generated by keyboard. Unable to handle it"
	}
}

#    putd {TEXT: <KeyPress> fired}
#	putd [string cat <<< %A >>>]
#	putd [string length %A]

# This is a modification of original binding from tk8.6/text.tcl 
bind Text <KeyPress> {
	CheckIfKeyPressIsNotReturn %A
	tk::TextInsert %W %A
}

# FIXME some options might be buffer-related, not text-related.
namespace eval ::clcon_text {

    ::snit::type opened_file {
        option -filename {}
        option -filemtime {}
    }
    
    variable GlobalPendingText2OduEventCounter
    if {![info exists GlobalPendingText2OduEventCounter]} {
        set GlobalPendingText2OduEventCounter 0
    }
    
    ::snit::widgetadaptor clcon_text {
        option -readonly -default 0
        # Send all editions to lisp
        option -send_to_lisp -default 0
        # Input is put into a special queue instead of applying to widget
        # This might be instance variable, but we need it in wrappers
        # This variable has three states: 0=normal,1=freezed,2=unfreezing
        option -private_freezed -default 0
        # Levels of nested freezing
        option -private_freeze_level 0 
        option -private_freezed_events_queue [list]
        # PRIVATE. Number of modifications sent which were not processed by oduvanchik yet
        option -private_pending_sent_modifications 0
        option -private_pending_far_tcl_continuations 0
        option -opened_file {}
        constructor {args} {
            installhull using btext
            $self configure -opened_file [opened_file $self.opened_file]
            # Apply any options passed at creation time.
            $self configurelist $args
            # Set FreezableText tags at first place (maybe should have placed to other place)
            set w [$self RealText]
            set CurrentBindTags [bindtags $w]
            set NewBindTags [SubstituteSingleValueInListVarKeyEq CurrentBindTags Text FreezableText]
            # set NewBindTags2 [lappend $NewBindTags TextTrackCursor]
            bindtags $win $NewBindTags
            bindtags $w $NewBindTags
            bind $win <<ContinueUnfreeze>> "$self Unfreeze"
            ::edt::CreateHighlightTags $self
            global $self.StatusBarInfo
            set $self.StatusBarInfo(CursorPos) {}
        }
        destructor {
            $options(-opened_file) destroy
            global $self.StatusBarInfo
            array unset $self.StatusBarInfo
            DestroyBackendBuffer $win
        }

        # Maybe disable the text widget's insert and delete methods, to
        # make this readonly.
        method insert {args} { if {!$options(-readonly)} { $self RoInsert {*}$args }}
        method delete {args} { if {!$options(-readonly)} { $self RoDelete {*}$args }}
        method replace {args} { if {!$options(-readonly)} { $self RoReplace {*}$args }}

        # Returns real text widget (this is not $self in case of btext)
        method RealText {} {
            return $self.t
        }
        
        # Enable synonyms, so the program can operate on readonly text
        method RoInsert {args} {
            putd "RoInsert $args"
            MaybeSendToLisp $self i $args
            set result [$hull insert {*}$args]
            return $result
        }
        method RoDelete {args} {
            putd "RoDelete $args"
            MaybeSendToLisp $self d $args
            set result [$hull delete {*}$args]
            return $result
        }
        method RoReplace {args} {
            MaybeSendToLisp $self r $args
            set result [$hull replace {*}$args]
            return $result
        }
        
        # NSL stands for "not send to lisp". Specially for oduvanchik commands which
        # do text editings, see do-editing-on-tcl-side.lisp
        method RoInsertNSL {args} {
            putd "RoInsertNSL $args"
            $hull insert {*}$args
        }
        method RoDeleteNSL {args} {
            putd "RoDeleteNSL $args"
            $hull delete {*}$args
        }
        # It looks like we don't need Replace at all
        
        method UsesLispP {} {
            variable ::tkcon::OPT
            return [expr {$OPT(oduvan-backend) && [$self cget -send_to_lisp]}]
        }
        
        method RememberEvent {script} {
            set q $options(-private_freezed_events_queue)
            # putd "444444 Remembering script $script"
            lappend q $script
            $self configure -private_freezed_events_queue $q
            #putd "[llength $q] events remembered]" 
        }

        method Freeze {} {
            # ::mprs::AssertEq $options(-private_freezed) 0 "Freeze: must be unfreezed"
            set old $options(-private_freezed)
            switch -exact $old {
                0 {
                    #putd "Called Freeze from 0. Will freeze"
                }
                1 { error "We need multi-level freezing" }
                2 {
                    #putd "I guess we called Freeze from Unfreeze. Lets freeze again"
                }
            }
            $self configure \
                -private_freezed 1 \
                -private_freeze_level [expr {1 + $options(-private_freeze_level)}]
        }
        
        method Unfreeze {} {
            #putd "Entering Unfreeze"
            if {!$options(-send_to_lisp)} {
                return
            }
            set pfl -private_freeze_level
            switch -exact $options($pfl) {
                0 { putd "Unfreeze: error - must be freezed" }
                1 {
                    if {$options($pfl)==1} {
                        $self configure -private_freezed 2
                        $self ContinueUnfreeze
                    }
                }
                default {
                    $self configure $pfl [expr {$options($pfl) - 1}]
                }
            }
        }

        method ContinueUnfreeze {} {
            #putd "Entering ContinueUnfreeze"
            if {!$options(-send_to_lisp)} {
                return
            }
            set pfl -private_freeze_level
            set current_freezed_state $options(-private_freezed)
            if {$options($pfl)>1} {
                # Someone have refrozen us. Do not unfreeze anymore, but decrease locking level                
                $self configure -private_freezed 1 \
                    $pfl [expr {$options($pfl) - 1}]
                return
            }
            switch -exact $current_freezed_state {
                0 {
                    puts stderr "Losing queued events: $options(-private_freezed_events_queue)"
                    $self configure -private_freezed_events_queue {}
                    error "ContinueUnfreeze: expected -private_freezed = 0 or 2. All queued events will be lost"
                }
                1 {
                    # do nothing - we are in freezed state again
                }
                2 {
                    set q $options(-private_freezed_events_queue)
                    if {[llength $q]} {
                        set script [lindex $q 0]
                        set q [lrange $q 1 end]
                        $self configure -private_freezed_events_queue $q
                        if {$script ne {}} {
                            # putd "444444 Unfreeze: about to eval $script"
                            catch {eval $script} code
                            if {$code ne {}} { putd "Error when unfreezing, will try to proceed: $code" }
                            # Check-the-world
                            if {!$options(-send_to_lisp)} {
                                return
                            }
                        }
                        #putd "Now will shedule ContinueUnfreeze"
                        after 50 event generate $win <<ContinueUnfreeze>>
                    } else {
                        # Event queue empty, state is 2. just check that level is 1 and exit
                        ::mprs::AssertEq 1 $options($pfl)
                        $self configure -private_freezed 0 $pfl [expr {$options($pfl) - 1}]
                        after idle [list ::clcon_text::tncm $self]
                    }
                }
            }
        }

        
        # Pass all other methods and options to the real text widget, so
        # that the remaining behavior is as expected.
        delegate method * to hull
        delegate option * to hull
    }

    proc lq {x} {
        ::tkcon::QuoteLispObjToString $x
    }
    
    proc ConstructBackendBuffer {clcon_text} {
        variable ::tkcon::OPT

        # If we want oduvan-backend, but there is no connection,
        # We won't try to communicate to lisp.
        # If later we connect to swank, the buffer will remain lame. 
        if {$::tkcon::OPT(oduvan-backend) && [::swcnn::SwankConnectedP]} {

            # this can be uncommented for debugging of the editor.
            # only first buffer's command are sent to lisp so that
            # less mess oduvanchik's state
	    #if {[::edt::Bi2btext "buf1"] ne $clcon_text} {
            #    return
            #}

            $clcon_text configure -send_to_lisp 1
            MaybeSendToLisp $clcon_text ConstructBackendBuffer {}
        }
    }

    proc DestroyBackendBuffer {clcon_text} {
        MaybeSendToLisp $clcon_text DestroyBackendBuffer {} {} 1
    }

    # Note that we have sent notification, or if notification
    # was processed on lisp side. Control number of pending events.
    # Args
    # increment - change of value
    # clcon_text - pathName of clcon_text widget
    # UseGlobalCounter - if 1, use global. If 0 - local. 
    proc IncrPendingSentNotifications {increment clcon_text UseGlobalCounter} {
        if {$UseGlobalCounter} {
            variable GlobalPendingText2OduEventCounter 
            incr GlobalPendingText2OduEventCounter $increment
            if {$GlobalPendingText2OduEventCounter>1} {
                # If we show this, something seem to be wrong
                showVarPutd GlobalPendingText2OduEventCounter
                putd "GlobalPendingText2OduEventCounter should be 0 or 1"
            }
            return $GlobalPendingText2OduEventCounter
        } else {
            if {![winfo exists $clcon_text]} {
                return 0
            }
            set j [$clcon_text cget -private_pending_sent_modifications]
            incr j $increment
            $clcon_text configure -private_pending_sent_modifications $j
            if {$j>1} {
                #putd "-private_pending_sent_modifications = $j"
            }
            return $j
        }
    }

    proc IncrPendingFarTclContinuations {increment clcon_text} {
        set j [$clcon_text cget -private_pending_far_tcl_continuations]
        incr j $increment
        $clcon_text configure -private_pending_far_tcl_continuations $j
        if {$j>1} {
            #putd "-private_pending_far_tcl_continuations = $j"
        }
        return $j
    }
    

    # ::clcon_text::MaybeSendToLisp
    # Sends to lisp an event reflecting changes in text widget, or requesting
    # to eval code on oduvanchik's side, or requesting to construct or destroy
    # backend buffer.
    # Args
    # clcon_text - pathname of clcon_text widget
    # type - type of event, "i", "d", "ConstructBackendBuffer", "DestroyBackendBuffer"
    # arglist - list of arguments of event (see code)
    # UseGlobalPendingText2OduEventCounter - if 1, this is not buffer-specific event
    # (destroy event in fact)
    proc MaybeSendToLisp {clcon_text type arglist {far_tcl_continuation_body {}} {UseGlobalPendingText2OduEventCounter 0}} {
        variable ::tkcon::OPT
        
        # This is a temporary solution. This is a separate option indeed
        set qId [lq $clcon_text]
        switch -exact $type {
            n {
                # see ::clcon_text::tncm
                if {![$clcon_text UsesLispP]} {
                    return
                }
                # When unfreezing, we DO NOT send it. ContinueUnfreeze would send it for us
                if {[$clcon_text cget -private_freezed]} {
                    return
                }
                set qIndex [::text2odu::CoerceIndex $clcon_text insert]
                set lispCmd "(clco:ncm $qId $qIndex)"
            }
            i {
                if {![$clcon_text UsesLispP]} {
                    return
                }
                if {[$clcon_text cget -private_freezed]} {
                    return
                }
                set index [lindex $arglist 0]
                set qIndex [::text2odu::CoerceIndex $clcon_text $index]
                set qText [lq [lindex $arglist 1]]
                set lispCmd "(clco:nti $qId $qIndex $qText)"
            }
            d {
                if {![$clcon_text UsesLispP]} {
                    return
                }
                if {[$clcon_text cget -private_freezed]} {
                    return
                }
                set b [lindex $arglist 0]
                set qB [::text2odu::CoerceIndex $clcon_text $b]
                set e [lindex $arglist 1]
                set qE [::text2odu::CoerceIndex $clcon_text $e]
                set lispCmd "(clco:notify-oduvan-tcl-text-delete $qId $qB $qE)"
            }
            ConstructBackendBuffer {
                if {![$clcon_text UsesLispP]} {
                    return
                }
                set lispCmd "(clco:notify-oduvan-construct-backend-buffer $qId)"
            }
            DestroyBackendBuffer {
                # We don't check if current buffer uses lisp, as it can not exist now already. But we at least should check is we have lisp at all.
                if {$OPT(oduvan-backend)} {
                    return
                }
                set lispCmd "(clco:notify-oduvan-destroy-backend-buffer $qId)"
            }
            CallOduvanchikFunction {
                if {![$clcon_text UsesLispP]} {
                    return
                }
                set qB [::text2odu::CoerceIndex $clcon_text insert]
                set FnAndArgs [lindex $arglist 0]
                set qOptions [::tkcon::QuoteTclListOfStringsForLisp [lindex $arglist 1]] 

                if {$far_tcl_continuation_body ne {}} {
                    ::clcon_text::IncrPendingFarTclContinuations 1 $clcon_text
                    set far_tcl_continuation_body [subst -nocommands {
                        $far_tcl_continuation_body
                        ::clcon_text::IncrPendingFarTclContinuations -1 $clcon_text
                    }]
                }

                # set qC [lq $far_tcl_continuation_body]
                set far_tcl_cont_id [::tkcon::GenContinuationCounter]
                ::mprs::EnqueueContinuation $far_tcl_cont_id $far_tcl_continuation_body
                set lispCmd "(clco:call-oduvanchik-function-with-clcon_text $qId $qB $far_tcl_cont_id '($FnAndArgs) '($qOptions))"
            }
            default {
                error "Hurray! We found replace command $type with args $clcon_text $type $arglist!"
            }
        }

        IncrPendingSentNotifications 1 $clcon_text $UseGlobalPendingText2OduEventCounter
        set continuation [subst -nocommands {
            ::clcon_text::IncrPendingSentNotifications \
                -1 $clcon_text $UseGlobalPendingText2OduEventCounter
        }]
        # putd "632017 MaybeSendToLisp: about to send $lispCmd with cont $continuation"
        ::tkcon::EvalInSwankAsync $lispCmd $continuation {:find-existing}
    }

    # This is "static" protection. And what if script calls function which returns code?
    # It can also return numeric value of a code, in which case we fail. 
    proc CheckIfScriptDoesNotContainBreakContinueOrReturn {script} {
        if {[string match *break* $script]} {
            error "script contains break: $script"
        }
        if {[string match *continue* $script]} {
            error "script contains continue: $script"
        }
        if {[string match *return* $script]} {
            error "script contains return: $script"
        }
    }
    
    # In freezable text, all event handler scripts must be processed
    # with this function to support freezing protocol.
    # Optional destination is required when event is bound not to
    # text itself, but, say, to menu bar. In this case destination
    # must expand to pathName of text.
    # Note: macro is not hygienic, $W can be captured, this is why
    # it is protected with namespace prefix.
    # If you need continue, add another named arg.
    proc WrapEventScriptForFreezedText {script args} {
        named_args $args {-destination "%W" -add-break 0 -note-cursor-motion 1}
        CheckIfScriptDoesNotContainBreakContinueOrReturn $script 
        set Template [lindex \
            {"if {[<<<<destination>>>> cget -private_freezed]} {
   <<<<destination>>>> RememberEvent {<<<<OldEventBody>>>>}
 } else {
   <<<<OldEventBody>>>><<<<NoteCursorMotion>>>>
 }<<<<MaybeBreak>>>>"} 0]

   # Inser this into script     
   # putd 444444
   # putd {<<<<     OldEventBody      >>>>}
        
        if {$(-note-cursor-motion)} {
            set NoteCursorMotion  "
 after idle [list ::clcon_text::tncm <<<<destination>>>>]"
        } else {
            set NoteCursorMotion ""
        }        
        if {$(-add-break)} {
            set MaybeBreak " 
 break"
        } else {
            set MaybeBreak ""
        }
        set t0 [regsub -all <<<<NoteCursorMotion>>>> $Template $NoteCursorMotion]
        set t1 [regsub -all <<<<OldEventBody>>>> $t0 $script]
        set t2 [regsub -all <<<<destination>>>> $t1 $(-destination)]
        set t3 [regsub -all <<<<MaybeBreak>>>> $t2 $MaybeBreak]
        # putd cannot be used here as this is called too early
        # puts stderr "WrapEventScriptForFreezedText234324: $t3"
        return $t3
    }

    # Try to arrange things so that correct and minimal set of cursor position
    # changes were noted. 
    proc DoesEventModifyCursorPos {ev} {
        # In tk_samples/original-tkcon.tcl, special bindtag is used for that
        # We found that for keyboard we will not achieve that bindtag. But
        # if our keyboard shortcuts call oduvanchik functions, we anyway note
        # cursor position change on unfreeze.
        # Maybe using DoesEventModifyCursorPos is a better approach, but
        # we have other problems now and can not afford resources to implement it.
        # FIXME
    }

    proc WrapFreezableHandlerScript {ev} {
        set script [bind Text $ev]
        set script2 [WrapEventScriptForFreezedText $script]
        bind FreezableText $ev $script2
    }

    # Fills FreezableText bindtag with wrapped bindings of Text
    # Tab and Shift-Tab bindings are bypassed - we do not like tabs in program code.
    proc InitBindingsOfFreezableText {} {
        foreach ev [bind Text] {
            if {[lsearch -exact {<Shift-Key-Tab> <Key-Tab>} $ev] != -1} {
                # Do nothing - these handlers contain "break" and they are not need as Tab will be overrided anyway
            } else {
                WrapFreezableHandlerScript $ev
            }
        }
        return
    }


    proc SubstituteSingleValueInListVarKeyEq {listVariable old new} {
        upvar 1 $listVariable var
        set idx [lsearch -exact $var $old]
        if {$idx >= 0} {
            set var [lreplace $var $idx $idx [list $new]]
        }
    }

    # UserContBody is a body of procedure to be called with two parameters: clcon_text and EventAsList. If supplied, it must call Unfreeze. 
    # OduvanchikFunctionNameAndArgs represents function from :oduvanchik home-package and its arguments, in a readable form. All packages to symbols must be specified. All string must be quoted with ::tkcon::QuoteTclStringForLisp. Subforms are not evaluated. Consider it as aquote list without outermost '()
    # Elements of list are passed to funcall. 
    # Options is a dict of options. Current known options are:
    # send_selection : if 1, selection is sent to odu at the beginning of command
    proc CallOduvanchikFunction {clcon_text OduvanchikFunctionNameAndArgs {UserContBody {}} {Options {}}} {
        variable ::tkcon::OPT
        if {![$clcon_text UsesLispP]} {
            error "Unable to call oduvanchik functions with oduvan-backend disabled (for this buffer)"
        }
        $clcon_text Freeze
        if {$UserContBody ne {}} {
            set ContBody [subst -nocommand {apply {{clcon_text EventAsList} $UserContBody} $clcon_text \$EventAsList}]
        } else {
            set ContBody [subst -nocommand {$clcon_text Unfreeze}]
        }
        showVarPutd ContBody
        MaybeSendToLisp $clcon_text CallOduvanchikFunction [list $OduvanchikFunctionNameAndArgs $Options] $ContBody
    }

    # Send cursor position to oduvanchik. This is for display purposes only, e.g. for
    # highlighting open parens. Normaly this function is programmed at after idle event.
    # When clcon_text is freezed, event is ignored. When unfreezing, it is send from
    # the unfreeze 2. 
    proc tncm {btext_or_btext_dot_t} {
        set x $btext_or_btext_dot_t
        # before idle came, widget could be destroyed.
        set clcon_text [::edt::CoerceTextToItsBText $x]
        if {![winfo exists $clcon_text]} {
            putd "Entered tncm:oops $clcon_text"
            return
        }
        global $clcon_text.StatusBarInfo
        set $clcon_text.StatusBarInfo(CursorPos) [$clcon_text index insert]
        MaybeSendToLisp $clcon_text n {}
    }

    # # To be called from oi::delete-region
    # proc clcon_text_delete_bb_ee {clcon_text} {
    #     putd stderr "Entered clcon_text_delete_bb_ee"
    #     $clcon_text delete bb ee
    #     $clcon_text mark unset bb
    #     $clcon_text mark unset ee
    #     putd stderr "About to exit clcon_text_delete_bb_ee"
    #     return {}
    # }

    # Oduvanchik is disconnected. Special care must be taken in the
    # case we have unfreezing processing on stack.
    proc OduvanchikDisconnected_clcon_text {clcon_text} {
        $clcon_text configure                          \
            -send_to_lisp 0                            \
            -private_freezed 0                         \
            -private_freeze_level 0                    \
            -private_freezed_events_queue [list]       \
            -private_pending_sent_modifications 0      \
            -private_pending_far_tcl_continuations 0
    }

    
    # InitOneBindingOfFreezableText <Key-Return>
    InitBindingsOfFreezableText
}

######################## Example of readonly text ###############
# See test/freezing-test-2.tcl

################### Example of freezing ########################
# See test/freezing-test-1.tcl


