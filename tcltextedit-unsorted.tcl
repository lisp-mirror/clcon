# Unsorted code borrowed from tcltextedit.

if { $::tkcon::OPT(putd-enabled) == 1 } {
    catch {destroy .logger}
    toplevel .logger
    wm title .logger "Logg"
    text .logger.text 
    pack .logger.text
    wm protocol  .logger WM_DELETE_WINDOW { destroy . }
}

proc c {args} {
    if { $::tkcon::OPT(putd-enabled) == 1 } {

        set result ""

	if {[info level]!=1} { 
            set result  "[info level -1] --> $args \n"

	} else { set result "Root --> $args"  }

	# if {$debug_messages==2} Make putd-enabled allow for 2? 
        if { 1 == 1 } { 
            .logger.text insert end $result
            .logger.text see end
	} else { puts stdout $result}

    }
}
