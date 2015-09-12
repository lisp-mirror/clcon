# ::clcon_text::clcon_text Widget based on example from snit FAQ
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

namespace eval ::clcon_text {
    ::snit::widgetadaptor clcon_text {
        option -readonly -default 0
        # Send all editions to lisp
        option -send_to_lisp -default 0
        # Input is put into a special queue instead of applying to widget
        # This might be instance variable, but we need it in wrappers
        option -private_freezed -default 0
        # It is private. Don't write to it
        option -private_freezed_events_queue [list]
        # PRIVATE. Number of modifications sent which were not processed by oduvanchik yet
        option -private_pending_sent_modifications 0
        constructor {args} {
            installhull using text
            # Apply any options passed at creation time.
            $self configurelist $args
            # Set FreezableText tags at first place (maybe should have placed to other place)
            set CurrentBindTags [bindtags $win]
            set NewBindTags [SubstituteSingleValueInListVarKeyEq CurrentBindTags Text FreezableText]
            bindtags $win $NewBindTags
            bind $win <<UnfreezeNext>> "$self Unfreeze"
        }
        destructor {
            DeleteBackendBufferForText $win
        }

        # Maybe disable the text widget's insert and delete methods, to
        # make this readonly.
        method insert {args} { if {!$options(-readonly)} { $self RoInsert {*}$args }}
        method delete {args} { if {!$options(-readonly)} { $self RoDelete {*}$args }}
        method replace {args} { if {!$options(-readonly)} { $self RoReplace {*}$args }}

        # Enable synonyms, so the program can operate on text
        # Pass all other methods and options to the real text widget, so
        # that the remaining behavior is as expected.
        method RoInsert {args} { set result [$hull insert {*}$args]
            MaybeSendToLisp $self i $args; return $result
        }
        method RoDelete {args} { set result [$hull delete {*}$args]
            MaybeSendToLisp $self d $args; return $result
        }
        method RoReplace {args} { set result [$hull replace {*}$args]
            MaybeSendToLisp $self r $args; return $result
        }

        method RememberEvent {script} {
            set q $options(-private_freezed_events_queue)
            #puts "Remembering script $script"
            lappend q $script
            $self configure -private_freezed_events_queue $q
            #puts "[llength $q] events remembered]" 
        }

        method Freeze {} {
            ::mprs::AssertEq $options(-private_freezed) 0 "Freeze: must be unfreezed"
            $self configure -private_freezed 1
        }
       
        method Unfreeze {} {
            set q $options(-private_freezed_events_queue)
            set script [lindex $q 0]
            set q [lrange $q 1 end]
            $self configure -private_freezed_events_queue $q
            ::mprs::AssertEq $options(-private_freezed) 1 "Unfreeze: must be freezed"
            if {$script ne {}} {
                #puts "Unfreeze: about to eval $script"
                eval $script
            }
            if {[llength $q]} {
                after 50 event generate $win <<UnfreezeNext>>
            } else {
                $self configure -private_freezed 0
            }
        }

        # Note that we have sent notification, or if notification was processed on lisp side
        method IncrPrivatePendingSentModifications {delta} {
            set j $options(-private_pending_sent_modifications)
            incr j $delta
            puts "-private_pending_sent_modifications = $j"
            $self configure -private_pending_sent_modifications $j
        }
        
        # Enqueue into associated event list
        delegate method * to hull
        delegate option * to hull
    }

    proc lq {x} {
        ::tkcon::QuoteLispObjToString $x
    }
   
    # Creates editor buffer in backend
    proc MakeBackendBufferForText {clcon_text} {
        variable ::tkcon::OPT
        set qClcon_text [lq $clcon_text]
        if $::tkcon::OPT(oduvan-backend) {
            $clcon_text configure -send_to_lisp 1
            ::tkcon::EvalInSwankAsync "(clco:make-oduvan-backend-buffer $qClcon_text)" {} 0
        }
    }

    proc DeleteBackendBufferForText {clcon_text} {
        puts "DeleteBackendBufferForText not written!"
    }

    proc MaybeSendToLisp {clcon_text type arglist} {
        variable ::tkcon::OPT
        if {![$clcon_text cget -send_to_lisp]
            ||
            !$::tkcon::OPT(oduvan-backend) } {
            return
        }
        set qId [lq $clcon_text]
        switch -exact $type {
            i {
                set index [lindex $arglist 0]
                set qIndex [::text2odu::CoerceIndex $clcon_text $index]
                set qText [lq [lindex $arglist 1]]
                set lispCmd "(clco:notify-oduvan-on-tcl-text-insert $qId $qIndex $qText)"
            }
            d {
                set b [lindex $arglist 0]
                set qB [::text2odu::CoerceIndex $clcon_text $b]
                set e [lindex $arglist 1]
                set qE [::text2odu::CoerceIndex $clcon_text $e]
                set lispCmd "(clco:notify-oduvan-on-tcl-text-delete $qId $qB $qE)"
            }
            default {
                error "Hurray! We found replace command with args $clcon_text $type $arglist!"
            }
        }
        $clcon_text IncrPrivatePendingSentModifications 1
        ::tkcon::EvalInSwankAsync $lispCmd [subst -nocommands {
            putd \$EventAsList
            $clcon_text IncrPrivatePendingSentModifications -1
        }] 0 t
            # FIXME maybe we need :find-existing? 
            # {:find-existing}
            # showVar arglist
            # set lispArglist [lmap a $arglist {
            #     ::tkcon::QuoteLispObjToString $a
            # }]
            # $clcon_text IncrPrivatePendingSentModifications 1
            # ::tkcon::EvalInSwankAsync \
            #     "(clco:notify-oduvan-on-tcl-text-change $type $lispArglist)" \
            #     [subst -nocommands {
            #         $clcon_text IncrPrivatePendingSentModifications -1
            #     }] 0
        puts "::clcon_text::MaybeSendToLisp: $clcon_text $type $arglist"
    }

    # In freezable text, all event handler scripts must be processed
    # with this function to support freezing protocol.
    # Optional destination is required when event is bound not to
    # text itself, but, say, to menu bar. In this case destination
    # must expand to pathName of text.
    # Note: macro is not hygienic, $W can be captured, this is why
    # it is protected with namespace prefix.
    proc WrapEventScriptForFreezedText {script {destination "%W"}} {
        set Template {
            if {[<<<<destination>>>> cget -private_freezed]} {
                <<<<destination>>>> RememberEvent {<<<<OldEventBody>>>>}
                break 
            } else {
                <<<<OldEventBody>>>>
            }
        }
        set t1 [regsub -all <<<<OldEventBody>>>> $Template $script]
        set t2 [regsub -all <<<<destination>>>> $t1 $destination]
        return $t2
    }


    # By calling this function we ensure freezing of the
    # buffer before processing the event.
    # Unfreezing must be arranged by event itself
    proc WrapFreezingAndFreezableHandlerScript {script} {
        error "write me like WrapEventScriptForFreezedText"
    }

    
    proc WrapFreezableHandlerScript {ev} {
        set script [bind Text $ev]
        set script2 [WrapEventScriptForFreezedText $script]
        bind FreezableText $ev $script2
    }
        

# Fills FreezableText bindtag with wrapped bindings of Text
    proc InitBindingsOfFreezableText {} {
        foreach ev [bind Text] {
            WrapFreezableHandlerScript $ev
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

    # InitOneBindingOfFreezableText <Key-Return>
    InitBindingsOfFreezableText
}

######################## Example of readonly text ###############
#  # Initialization (readonly set to 1)
# ::clcon_text::clcon_text .text -readonly 1
#  # Changing readonly after creation
# .text configure -readonly 0
# .text configure -readonly 1
    
# .text insert 0.0 "this line will be ignored"

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
