package require Tk
package require snit

namespace eval ::clcon_text {
    # Widget based on example from snit FAQ
    
    # If -readonly option == 1, this makes text widget readonly,
    # keeping an ability to receive focus
    # and a visible cursor for keyboard navigation.
    # Readonly text
    # text can still be manipulated with
    # RoInsert, RoDelete, RoReplace
    #
    # If -readonly option == 0, widget can be operated normally, and
    # adds RoInsert, RoDelete, RoReplace subprocedures (or how
    # they are called here in tcl) are available too
    # If -send_to_lisp options is set, all text modifications are
    # sent to lisp (to be implemented)
    ::snit::widgetadaptor clcon_text {
        option -readonly -default 0
        # Send all editions to lisp
        option -send_to_lisp -default 0
        # Input is put into a special queue instead of applying to widget
        # This might be instance variable, but we need it in wrappers
        option -private_freezed -default 0
        # It is private. Don't write to it
        option -private_freezed_events_queue [list]

        constructor {args} {
            installhull using text
            # Apply any options passed at creation time.
            $self configurelist $args
            # Set FreezableText tags at first place (maybe should have placed to other place)
            bindtags $win "FreezableText [bindtags $win]"
            bind $win <<UnfreezeNext>> "$self Unfreeze"
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
                set $options(-private_freezed) 0
            }
        }

        # Enqueue into associated event list
        delegate method * to hull
        delegate option * to hull
    }

    proc MaybeSendToLisp {clcon_text type arglist} {
        if {[$clcon_text cget -send_to_lisp]} {
            puts "::clcon_text::MaybeSendToLisp: $clcon_text $type $args"
        }
    }

    proc InitOneBindingOfFreezableText {ev} {
        set body [bind Text $ev]
        set Template {
            if {[%W cget -private_freezed]} {
                %W RememberEvent {<<<<OldEventBody>>>>}
            } else {
                <<<<OldEventBody>>>>
            }
        # FIXME - remove that later!
            break 
        }
        set ExpandedBody [regsub -all <<<<OldEventBody>>>> $Template $body]
        showVar ExpandedBody
        bind FreezableText $ev $ExpandedBody
    }

        

# Fills FreezableText bindtag with wrapped bindings of Text
    proc InitBindingsOfFreezableText {} {
        foreach ev [bind Text] {
            InitOneBindingOfFreezableText $ev
        }
    }

    InitOneBindingOfFreezableText <Key-Return>
    
}
     
######################## Example ################################

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
