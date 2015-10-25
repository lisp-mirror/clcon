# Unsorted code borrowed from tcltextedit.

proc MaybeCreateLogger {} {
    if { $::tkcon::OPT(putd-enabled) == 1 } {
        catch {destroy .logger}
        toplevel .logger
        wm title .logger "Logg"
        text .logger.text 
        pack .logger.text
        # wm protocol  .logger WM_DELETE_WINDOW { destroy . }
    }
}

proc c {args} {
    if { $::tkcon::OPT(putd-enabled) == 1 } {

        set result ""

	if {[info level]!=1} { 
            set result  "[info level -1] --> $args \n"

	} else { set result "Root --> $args"  }

	# if {$debug_messages==2} Make putd-enabled allow for 2? 
        if { 1 == 0 } {
            MaybeCreateLogger
            .logger.text insert end $result
            .logger.text see end
	} else { putd $result}

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


proc MakeWindowADialogAndGrab {win parent} {
    global tcl_platform
    if {$tcl_platform(platform) == "unix"} {
        wm attributes $win -type dialog
    } elseif {$tcl_platform(platform) == "windows"} {
        # do nothing
    }
    wm transient $win $parent

    #if {$parent ne {}} {
    #    powin $win $parent
    #}

    raise $win
    grab $win
}


# This is very minimalistic feature, but it is good as it
# does not bother swank server. This is essential at debug time.
# Returns list of two elements:
# First element is "ok" or "cancel".
# Second is a line entered (if result is "ok")
proc LameAskForLispExpression {parent title} {
    global LameAskForLispExpressionReply
    set ou [string cat $parent .ask_for_lisp_expression]
    catch {destroy $ou}
    toplevel $ou
    wm title $ou $title
    set text $ou.text
    text $text -height 3
    ::gui_util::ConfigureTextFonts $text
    pack $text
    set LameAskForLispExpressionReply 0
    bind $text <Return> "set LameAskForLispExpressionReply ok; break"
    bind $text <Escape> "set LameAskForLispExpressionReply cancel"
    bind $text <Shift-Return> "$text insert insert \\n; break"

    wm protocol $ou WM_DELETE_WINDOW "set LameAskForLispExpressionReply cancel"

    MakeWindowADialogAndGrab $ou $parent

    focus $text
    
    vwait LameAskForLispExpressionReply
    set userInput [$text get 1.0 end-1c]
    if {$LameAskForLispExpressionReply eq "ok"} {
        set result [list $LameAskForLispExpressionReply $userInput]
    } else {
        set result [list $LameAskForLispExpressionReply]
    }
    destroy $ou
    return $result
}

# Has local grab, returns 'yes','no' or 'cancel' 
proc YesNoCancel {title message parent} {
    global YesNoCancelReply
    set ou [string cat $parent .ou]
    catch {destroy $ou}
    toplevel $ou
    wm withdraw $ou
    wm title $ou $title
    set YesNoCancelReply "cancel"
    frame $ou.f
    label $ou.label -text $message
    pack [button $ou.f.yes -text "1.Yes" -underline 0 \
              -command "set YesNoCancelReply yes" -width 5]
    bind $ou.f.yes <Return> "set YesNoCancelReply yes"
    pack [button $ou.f.no -text "2.No" -underline 0 \
              -command "set YesNoCancelReply no" -width 5]
    bind $ou.f.no <Return> "set YesNoCancelReply no"
    pack [button $ou.f.cancel  -text "3.Cancel" -underline 0 \
              -command "set YesNoCancelReply cancel" -width 5]
    bind $ou.f.cancel <Return> "set YesNoCancelReply cancel"
    pack $ou.label -side top
    pack $ou.f.yes $ou.f.no $ou.f.cancel -side left  -padx 10
    bind $ou <Key-2> "set YesNoCancelReply no"
    bind $ou <Key-1> "set YesNoCancelReply yes"
    bind $ou <Key-3> "set YesNoCancelReply cancel"
    bind $ou <Escape> "set YesNoCancelReply cancel"
    #label $ou.img -image warn
    pack $ou.f
    wm deiconify $ou
    raise $ou
    focus $ou.f.yes
    wm protocol $ou WM_DELETE_WINDOW "set YesNoCancelReply cancel"

    MakeWindowADialogAndGrab $ou $parent

    vwait YesNoCancelReply
    destroy $ou
    return $YesNoCancelReply
}
