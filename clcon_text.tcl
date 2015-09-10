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
        option -queue_input -default 0
        constructor {args} {
            # Create the text widget; turn off its insert cursor
            installhull using ctext
            # Apply any options passed at creation time.
            $self configurelist $args
        }
        # Maybe disable the text widget's insert and delete methods, to
        # make this readonly.
        method insert {args} {
            if {!$options(-readonly)} {
                $self RoInsert {*}$args
            }
        }
        method delete {args} {
            if {!$options(-readonly)} {
                $self RoDelete {*}$args
            }
        }
        method replace {args} {
            if {!$options(-readonly)} {
                $self RoReplace {*}$args
            }
        }
        # Enable synonyms, so the program can operate on text
        # Pass all other methods and options to the real text widget, so
        # that the remaining behavior is as expected.
        method RoInsert {args} {
            set result [$hull insert {*}$args]
            if {[$self cget -send_to_lisp]} {
                puts "Sending to lisp: i $args"                    
            }
            return $result
        }
        method RoDelete {args} {
            set result [$hull delete {*}$args]
            if {[$self cget -send_to_lisp]} {
                puts "Sending to lisp: d $args"
            }
            return $result
        }
        method RoReplace {args} {
            set result [$hull replace {*}$args]
            if {[$self cget -send_to_lisp]} {
                puts "Sending to lisp: r $args"
            }
            return $result
        }
        delegate method * to hull
        delegate option * to hull
    }


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
