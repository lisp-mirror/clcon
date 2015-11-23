## Completions menu

proc ::gui_util::scrollable_menu_path_to_tbl {tl} {
    return $tl.tf.tbl
}

proc ::gui_util::scrollable_menu_do_return {tl} {
    variable call_scrollable_menu_return_flag 
    variable call_scrollable_menu_return_value
    set tbl [scrollable_menu_path_to_tbl $tl]
    set call_scrollable_menu_return_value [$tbl get active active]
    set call_scrollable_menu_return_flag 1    
}
    
proc ::gui_util::scrollable_menu_do_cancel {find} {
    variable call_scrollable_menu_return_flag 
    variable call_scrollable_menu_return_value
    set tbl [scrollable_menu_path_to_tbl $tl]
    set call_scrollable_menu_return_value {}
    set call_scrollable_menu_return_flag 1    
}


# Billet only
# Args: items is a list
# Returns: item selected or "" is Esc (Control-w) pressed
proc ::gui_util::call_scrollable_menu {items args} {
    named_args $args {-owner {} -title "::gui_util::call_scrollable_menu" -width 40}

    # rename to generated toplevel id
    variable call_scrollable_menu_return_flag 0
    variable call_scrollable_menu_return_value {}

    set tl .scrollable_menu

    catch {destroy $tl}

    toplevel $tl
    wm title $tl $(-title)

    frame $tl.tf
    set tbl [scrollable_menu_path_to_tbl $tl]
        
    tablelist::tablelist $tbl -columns [list $(-width) "Invisible label"] -stretch 0 \
        -showlabels 0 \
        -foreground \#000000 \
        -font tkconfixed -borderwidth 1 -highlightthickness 0 \
        -width [expr {$(-width)+2}]

    $tbl columnconfigure 0 -wrap true

    $tbl insertlist 0 $items

    set f1 $tl.tf
    scrollbar $f1.sy -orient v -command [list $tbl yview]
    $tbl configure -yscrollcommand [list $f1.sy set]
    grid $tbl - $f1.sy -sticky news
    grid columnconfigure $f1 0 -weight 1
    grid columnconfigure $f1 1 -weight 1
    grid rowconfigure $f1 0 -weight 1

    pack $f1 -side top -fill both -expand 1

    set esc_binding "after 0 ::gui_util::scrollable_menu_do_cancel [list $tl]"

    bind $tl <Escape> "$esc_binding ; break"
    ::clcon_key::b bind $tl <Control-Key-w> "$esc_binding ; break"

    set ok_binding "after 0 ::gui_util::scrollable_menu_do_return [list $tl]; break"

    bind $tl <Return> $ok_binding
    bind $tl <Double-1> $ok_binding

    wm protocol $tl WM_DELETE_WINDOW $esc_binding

    ::tablelist_util::GotoIndex $tbl 0 

    focus $tbl
    grab $tl

    set call_scrollable_menu_return_value {}

    vwait ::gui_util::call_scrollable_menu_return_flag

    catch {destroy $tl}

    if {$(-owner) ne {}} {
        focus $(-owner)
    }

    return $call_scrollable_menu_return_value

}
