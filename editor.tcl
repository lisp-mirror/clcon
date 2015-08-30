# This will be an option
# If true, we allow for only one editor window at a time, joungling frames in it
# New window to the same place where old one was
proc SingleEditorWindow {} {
    return 1
}

# We have global variables to keep window list
proc InitEditorWindowDict {} {
    global EditorWindowDict
    if {[info vars EditorWindowDict] eq ""} {
        global EditorWindowCounter
        set EditorWindowDict {}
        set EditorWindowCounter 0
    }
}

InitEditorWindowDict

proc GenEditorWindowName {} {
    variable ::tkcon::PRIV
    global EditorWindowCounter 
    set EditorWindowCounter [expr {$EditorWindowCounter + 1}]
    return [string cat $PRIV(base).__edit $EditorWindowCounter]
}

# FIXME keep a bucket of windows for given ArgsToIdentifyWindow
# Then fix HideAllEditorWindows also. 
proc AddToWindowDict {w ArgsToIdentifyWindow} {
    global EditorWindowDict
    set i 0
    if { [dict exists "$EditorWindowDict" $ArgsToIdentifyWindow ] } {
        putd "Handling of duplicating editor args unimplemented!"
    }
    dict append EditorWindowDict [list $ArgsToIdentifyWindow $w]
}

# Find frame with the same args and let the user define
# to use it or to create a new one. If user chosen to create new one,
# return {}
proc ReuseEditorWindow {ArgsToIdentifyWindow word opts} {
    return {}
}

# Returns a new frame for new window
proc MakeNewEditorWindowName {ArgsToIdentifyWindow word opts} {
    set tw [GenEditorWindowName]
    set w $tw

    # tw and w are now the same, but we will store frame name, not a window name
    AddToWindowDict $w $ArgsToIdentifyWindow
    return $tw
}


proc HideAllEditorWindows {} {
    global EditorWindowDict
    dict for {k v} $EditorWindowDict {
        set window [lindex $v 1]
        if {[winfo exists $window]} {
            wm withdraw $window
        }
    }
}

# Initializes editor GUI
# args are for error only
proc InitEditorWindow {tw w ArgsToIdentifyWindow word opts args} {
    variable ::tkcon::PRIV
    variable ::tkcon::COLOR
    variable ::tkcon::OPT

    if {[string length $word] > 20} {
        wm title $tw "[string range $word 0 16]... - tkcon Edit"
    } else {
        wm title $tw "$word - tkcon Edit"
    }

    puts "w.text = $w.text"
    set txt [text $w.text]
    $w.text configure -wrap [dict get $opts -wrap] \
        -xscrollcommand [list $w.sx set] \
        -yscrollcommand [list $w.sy set] \
        -foreground $COLOR(stdin) \
        -background $COLOR(bg) \
        -insertbackground $COLOR(cursor) \
        -font $::tkcon::OPT(font) -borderwidth 1 -highlightthickness 0 \
        -undo 1
    catch {
        # 8.5+ stuff
        set tabsp [expr {$OPT(tabspace) * [font measure $OPT(font) 0]}]
        $w.text configure -tabs [list $tabsp left] -tabstyle wordprocessor
    }

    scrollbar $w.sx -orient h -command [list $w.text xview]
    scrollbar $w.sy -orient v -command [list $w.text yview]
    
    set menu [menu $w.mbar]
    $w configure -menu $menu
    
    ## File Menu
    ##
    set m [menu [::tkcon::MenuButton $menu File file]]
    $m add command -label "Save As..."  -underline 0 \
        -command [list ::tkcon::Save {} widget $w.text]
    $m add command -label "Append To..."  -underline 0 \
        -command [list ::tkcon::Save {} widget $w.text a+]
    $m add separator
    $m add command -label "Dismiss" -underline 0 -accel $PRIV(ACC)w \
        -command [list destroy $w]
    bind $w <$PRIV(CTRL)w>		[list destroy $w]
    bind $w <Alt-w>		[list destroy $w]
    
    ## Edit Menu
    ##
    set text $w.text
    set m [menu [::tkcon::MenuButton $menu Edit edit]]
    $m add command -label "Cut"   -under 2 \
        -command [list tk_textCut $text]
    $m add command -label "Copy"  -under 0 \
        -command [list tk_textCopy $text]
    $m add command -label "Paste" -under 0 \
        -command [list tk_textPaste $text]
    $m add separator
    $m add command -label "Find" -under 0 \
        -command [list ::tkcon::FindBox $text]
    
    ## Send To Menu
    ## 
    # Try to keep Send menu by allowing to send to main interpreter only
    set m [menu [::tkcon::MenuButton $menu "Send to..." send]]
    set other [tkcon attach]
    $m add command -label "Send To [lindex $other 0]" \
        -command "::tkcon::EvalOther $other \
		    eval \[$w.text get 1.0 end-1c\]"
    
    grid $w.text - $w.sy -sticky news
    grid $w.sx - -sticky ew
    grid columnconfigure $w 0 -weight 1
    grid columnconfigure $w 1 -weight 1
    grid rowconfigure $w 0 -weight 1

    
    
    switch -glob -- [dict get $opts -type] {
        proc*	{
            $w.text insert 1.0 \
                [::tkcon::EvalOther {} slave dump proc [list $word]]
            after idle [::tkcon::Highlight $w.text tcl]
        }
        var*	{
            $w.text insert 1.0 \
                [::tkcon::EvalOther {} slave dump var [list $word]]
            after idle [::tkcon::Highlight $w.text tcl]
        }
        file	{
            $w.text insert 1.0 [::tkcon::EvalOther {} slave eval \
                                    [subst -nocommands {
                                        set __tkcon(fid) [open {$word} r]
                                        set __tkcon(data) [read \$__tkcon(fid)]
                                        close \$__tkcon(fid)
                                        after 1000 unset __tkcon
                                        return \$__tkcon(data)
                                    }
                                    ]]
            after idle [::tkcon::Highlight $w.text \
                            [string trimleft [file extension $word] .]]
        }
        error*	{
            $w.text insert 1.0 [join $args \n]
            after idle [::tkcon::Highlight $w.text error]
        }
        default	{
            $w.text insert 1.0 [join $args \n]
        }
    }
}


proc EnsureEditorWindow {tw} {
    variable ::tkcon::PRIV
    if {![winfo exists $tw]} {
        toplevel $tw
        wm withdraw $tw
     }
    return $tw
}    


## edit - opens a file/proc/var for reading/editing
## 
# Arguments:
#   type	proc/file/var
#   what	the actual name of the item
# Returns:	nothing
# Added by budden : -offset option, accepts index
##
proc edit {args} {
    variable ::tkcon::PRIV
    variable ::tkcon::COLOR
    variable ::tkcon::OPT

    HideAllEditorWindows
    
    set ArgsToIdentifyWindow $args
    
    set opts [dict create -find {} -type {} -attach {} -wrap {none} -offset {}]             
    while {[string match -* [lindex $args 0]]} {
	switch -glob -- [lindex $args 0] {
	    -f*	{ dict set opts -find [lindex $args 1] }
	    -a*	{ dict set opts -attach [lindex $args 1] }
	    -t*	{ dict set opts -type [lindex $args 1] }
	    -w*	{ dict set opts -wrap [lindex $args 1] }
            -o* { dict set opts -offset [lindex $args 1] }
	    #--	{ set args [lreplace $args 0 0]; break }
            --	{ break }
	    default {return -code error "unknown option \"[lindex $args 0]\""}
	}
	set args [lreplace $args 0 1]
    }
    # determine who we are dealing with
    if {[llength [ dict get $opts -attach]]} {
	foreach {app type} [ dict get $opts -attach] {break}
    } else {
	foreach {app type} [tkcon attach] {break}
    }

    set word [lindex $args 0]

    if {[dict get $opts -type] == {}} {
	if {[llength [::tkcon::EvalOther $app $type info commands [list $word]]]} {
	    dict set opts -type "proc"
	} elseif {[llength [::tkcon::EvalOther $app $type info vars [list $word]]]} {
	    dict set opts -type "var"
	} elseif {[::tkcon::EvalOther $app $type file isfile [list $word]]} {
	    dict set opts -type "file"
	}
    }
    if {[dict get $opts -type] == {}} {
	return -code error "unrecognized type '$word'"
    }

    # In the future, tw will stand for window, w - for frame
    # Now they coincide
     
    # Find old edit window if there is one
    set tw [::ReuseEditorWindow $ArgsToIdentifyWindow $word $opts]
    set w $tw

    # If not, create one
    if {$tw eq {}} {
        set tw [::MakeNewEditorWindowName $ArgsToIdentifyWindow $word $opts]
        ::EnsureEditorWindow $tw
        set w $tw
        ::InitEditorWindow $tw $w $ArgsToIdentifyWindow $word $opts $args
    }

    wm deiconify $tw
        
    focus -force $w.text
        
    if {[string compare [dict get $opts -find] {}]} {
	::tkcon::Find $w.text [dict get $opts -find] -case 1
    }
    if {[dict get $opts -offset] ne {}} {
	$w.text mark set insert [dict get $opts -offset]
        $w.text see insert
    }
}
