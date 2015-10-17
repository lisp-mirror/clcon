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

# FIXME some options might be buffer-related, not text-related.
namespace eval ::clcon_text {

    snit::type opened_file {
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
        # It is private. Don't write to it
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
            set CurrentBindTags [bindtags $win]
            set NewBindTags [SubstituteSingleValueInListVarKeyEq CurrentBindTags Text FreezableText]
            bindtags $win $NewBindTags
            bind $win <<ContinueUnfreeze>> "$self Unfreeze"
            ::edt::CreateHighlightTags $self
        }
        destructor {
            $options(-opened_file) destroy
            DestroyBackendBuffer $win
        }

        # Maybe disable the text widget's insert and delete methods, to
        # make this readonly.
        method insert {args} { if {!$options(-readonly)} { $self RoInsert {*}$args }}
        method delete {args} { if {!$options(-readonly)} { $self RoDelete {*}$args }}
        method replace {args} { if {!$options(-readonly)} { $self RoReplace {*}$args }}

        # Enable synonyms, so the program can operate on readonly text
        method RoInsert {args} {
            MaybeSendToLisp $self i $args
            set result [$hull insert {*}$args]
            return $result
        }
        method RoDelete {args} {
            MaybeSendToLisp $self d $args
            set result [$hull delete {*}$args]
            return $result
        }
        method RoReplace {args} {
            MaybeSendToLisp $self r $args
            set result [$hull replace {*}$args]
            return $result
        }

        # NSL stands for "not send to lisp". Especially for oduvanchik commands which
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

        method RememberEvent {script} {
            set q $options(-private_freezed_events_queue)
            putd "444444 Remembering script $script"
            lappend q $script
            $self configure -private_freezed_events_queue $q
            #putd "[llength $q] events remembered]" 
        }

        method Freeze {} {
            # ::mprs::AssertEq $options(-private_freezed) 0 "Freeze: must be unfreezed"
            set old $options(-private_freezed)
            switch -exact $old {
                0 {
                    putd "Called Freeze from 0. Will freeze"
                }
                1 { error "We need multi-level freezing" }
                2 {
                    putd "I guess we called Freeze from Unfreeze. Lets freeze again"
                }
            }
            $self configure -private_freezed 1
        }
        
        method Unfreeze {} {
            putd "Entering Unfreeze"
            set current_freezed_state $options(-private_freezed)
            ::mprs::AssertEq [expr {$current_freezed_state>0}] 1 "Unfreeze: must be freezed"
            showVarPutd current_freezed_state
            switch -exact $current_freezed_state {
                0 { putd "Unfreeze: error - wrong state 0. Can continue" }
                1 { $self configure -private_freezed 2
                    $self ContinueUnfreeze
                }
                2 { error "Unfreeze: it looks like we need multi-level freezing counter"
                }
            }
        }

        method ContinueUnfreeze {} {
            putd "Entering ContinueUnfreeze"
            set current_freezed_state $options(-private_freezed)
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
                    set script [lindex $q 0]
                    set q [lrange $q 1 end]
                    $self configure -private_freezed_events_queue $q
                    if {$script ne {}} {
                        putd "444444 Unfreeze: about to eval $script"
                        catch {eval $script} code
                        if {$code ne {}} { putd "Error when unfreezing, will try to proceed: $code" }
                    }
                    if {[llength $q]} {
                        putd "Now will shedule ContinueUnfreeze"
                        after 50 event generate $win <<ContinueUnfreeze>>
                    } else {
                        putd "Finished unfreezing"
                        $self configure -private_freezed 0
                    }
                }
            }
        }

        
        # Enqueue into associated event list
        delegate method * to hull
        delegate option * to hull
    }

    proc lq {x} {
        ::tkcon::QuoteLispObjToString $x
    }
    
    proc ConstructBackendBuffer {clcon_text} {
        variable ::tkcon::OPT
        if $::tkcon::OPT(oduvan-backend) {
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
                putd "-private_pending_sent_modifications = $j"
            }
            return $j
        }
    }

    proc IncrPendingFarTclContinuations {increment clcon_text} {
        set j [$clcon_text cget -private_pending_far_tcl_continuations]
        incr j $increment
        $clcon_text configure -private_pending_far_tcl_continuations $j
        if {$j>1} {
            putd "-private_pending_far_tcl_continuations = $j"
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
        if {!$::tkcon::OPT(oduvan-backend) } {
            return
        }
        # This is a temporary solution. This is a separate option indeed
        set qId [lq $clcon_text]
        switch -exact $type {
            i {
                if {[$clcon_text cget -private_freezed]
                    ||
                    ![$clcon_text cget -send_to_lisp] } {
                    return
                }
                set index [lindex $arglist 0]
                set qIndex [::text2odu::CoerceIndex $clcon_text $index]
                set qText [lq [lindex $arglist 1]]
                set lispCmd "(clco:nti $qId $qIndex $qText)"
            }
            d {
                if {[$clcon_text cget -private_freezed]
                    ||
                    ![$clcon_text cget -send_to_lisp] } {
                    return
                }
                set b [lindex $arglist 0]
                set qB [::text2odu::CoerceIndex $clcon_text $b]
                set e [lindex $arglist 1]
                set qE [::text2odu::CoerceIndex $clcon_text $e]
                set lispCmd "(clco:notify-oduvan-tcl-text-delete $qId $qB $qE)"
            }
            ConstructBackendBuffer {
                set lispCmd "(clco:notify-oduvan-construct-backend-buffer $qId)"
            }
            DestroyBackendBuffer {
                set lispCmd "(clco:notify-oduvan-destroy-backend-buffer $qId)"
            }
            CallOduvanchikFunction {
                if {![$clcon_text cget -send_to_lisp] } {
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
        putd "MaybeSendToLisp: about to send $lispCmd with cont $continuation"
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
        named_args $args {-destination "%W" -add-break 0}
        CheckIfScriptDoesNotContainBreakContinueOrReturn $script 
        set Template [lindex \
            {"if {[<<<<destination>>>> cget -private_freezed]} {\
   <<<<destination>>>> RememberEvent {<<<<OldEventBody>>>>}\
 } else {\
   putd 444444
   putd {<<<<OldEventBody>>>>}
   <<<<OldEventBody>>>>\
 }<<<<MaybeBreak>>>>"} 0]
        if {$(-add-break)} {
            set MaybeBreak "\
 break"
        } else {
            set MaybeBreak ""
        }
        set t1 [regsub -all <<<<OldEventBody>>>> $Template $script]
        set t2 [regsub -all <<<<destination>>>> $t1 $(-destination)]
        set t3 [regsub -all <<<<MaybeBreak>>>> $t2 $MaybeBreak]
        return $t3
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

    # UserContBody is a body of procedure to be called with two parameters: clcon_text and EventAsList. Beware UserContBody is called after unfreezing!
    # OduvanchikFunctionNameAndArgs represents function from :oduvanchik home-package and its arguments, in a readable form. All packages to symbols must be specified. All string must be quoted with ::tkcon::QuoteTclStringForLisp. Subforms are not evaluated. Consider it as aquote list without outermost '()
    # Elements of list are passed to funcall. 
    # Options is a dict of options. Current known options are:
    # send_selection : if 1, selection is sent to odu at the beginning of command
    proc CallOduvanchikFunction {clcon_text OduvanchikFunctionNameAndArgs {UserContBody {}} {Options {}}} {
        variable ::tkcon::OPT
        if {!$::tkcon::OPT(oduvan-backend)} {
            error "Unable to call oduvanchik functions with oduvan-backend disabled"
        }
        $clcon_text Freeze
        if {$UserContBody ne {}} {
            set ContBody [subst -nocommand {$clcon_text Unfreeze; apply {{clcon_text EventAsList} $UserContBody} $clcon_text \$EventAsList}]
        } else {
            set ContBody [subst -nocommand {$clcon_text Unfreeze}]
        }
        showVarPutd ContBody
        MaybeSendToLisp $clcon_text CallOduvanchikFunction [list $OduvanchikFunctionNameAndArgs $Options] $ContBody
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

    # InitOneBindingOfFreezableText <Key-Return>
    InitBindingsOfFreezableText
}

######################## Example of readonly text ###############
#  # Initialization (readonly set to 1)
# ::clcon_text::clcon_text .text -readonly 1
#  # Changing readonly after creation
# .text configure -readonly 0
# .text configure -readonly 1

# .text insert 1.0 "this line will be ignored"

# for {set x 0} {$x<10} {incr x} { 
#     .text RoInsert end "line $x\n"
# }

# ::clcon_text::clcon_text .text2
# .text2 insert end "Line inserted by 'insert'\n"
# .text2 RoInsert end "Line inserted by 'RoInsert'\n"

# pack .text   -side top
# pack .text2 -side bottom


################### Example of freezing ########################
# catch { destroy .freezingExample }
# toplevel .freezingExample

# set txt .freezingExample.text
# ::clcon_text::clcon_text $txt

# for {set x 0} {$x<10} {incr x} { 
#     $txt insert end "line $x\n"
# }

# pack $txt -side top -fill both
# focus $txt

# # Do .freezingExample.text Freeze
# # to start buffering input
# # Do .freezingExample.text Unfreeze
# # to finish

############ Test case in clcon ###########
#..set ed [::edt::edit /s2/clcon/clcon_text.tcl]
#..$ed Freeze
# Now type something and try to do something
#..$ed Unfreeze
