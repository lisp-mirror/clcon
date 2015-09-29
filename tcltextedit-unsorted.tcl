# Unsorted code borrowed from tcltextedit.

proc MaybeCreateLogger {} {
    if { $::tkcon::OPT(putd-enabled) == 1 } {
        catch {destroy .logger}
        toplevel .logger
        wm title .logger "Logg"
        text .logger.text 
        pack .logger.text
        wm protocol  .logger WM_DELETE_WINDOW { destroy . }
    }
}

proc c {args} {
    if { $::tkcon::OPT(putd-enabled) == 1 } {

        set result ""

	if {[info level]!=1} { 
            set result  "[info level -1] --> $args \n"

	} else { set result "Root --> $args"  }

	# if {$debug_messages==2} Make putd-enabled allow for 2? 
        if { 1 == 1 } {
            MaybeCreateLogger
            .logger.text insert end $result
            .logger.text see end
	} else { puts stdout $result}

    }
}

proc tkTextSetCursor {text index} {
    $text mark set insert $index
}


proc powin {w relativeTo} {
    set y [winfo y $relativeTo]
    #set x [expr [winfo x .] + ([winfo width .]/2)  - ($wid/2) ]
    set x [expr [winfo x $relativeTo] + ([winfo width $relativeTo]/2)  - (200) ]
    wm geometry $w "=+$x+$y" 
    c [winfo width $w]
}

# Has local grab, returns 'yes','no' or 'cancel' 
proc YesNoCancel {title message parent} {
    global r
    set ou [string cat $parent .ou]
    catch {destroy $ou}
    toplevel $ou
    wm title $ou $title
    set r "cancel"
    frame $ou.f
    label $ou.label -text $message
    pack [button $ou.f.yes -text "1.Yes" -underline 0      -command "set r yes" -width 5]
    bind $ou.f.yes <Return> "set r yes"
    pack [button $ou.f.no -text "2.No" -underline 0 -command "set r no" -width 5]
    bind $ou.f.no <Return> "set r no"
    pack [button $ou.f.cancel  -text "3.Cancel" -underline 0        -command "set r cancel" -width 5]
    bind $ou.f.cancel <Return> "set r cancel"
    pack $ou.label -side top
    pack $ou.f.yes $ou.f.no $ou.f.cancel -side left  -padx 10
    bind $ou <Key-2> "set r no"
    bind $ou <Key-1> "set r yes"
    bind $ou <Key-3> "set r cancel"
    bind $ou <Escape> "set r cancel"
    #label $ou.img -image warn
    pack $ou.f
    focus $ou.f.yes
    wm protocol $ou WM_DELETE_WINDOW "set r cancel"
    wm attributes $ou -type dialog
    wm transient $ou $parent

    #if {$parent ne {}} {
    #    powin $ou $parent
    #}
    grab $ou
    vwait r
    destroy $ou
    return $r
}
