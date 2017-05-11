## Completions menu

namespace eval ::completions_menu {
    variable return_flag 
    variable return_value
}

proc ::completions_menu::path_to_tbl {tl} {
    return $tl.tf.tbl
}

proc ::completions_menu::do_return {tl} {
    variable return_flag 
    variable return_value
    set tbl [path_to_tbl $tl]
    set return_value_overquoted [$tbl get active active]
    set return_value [concat {*}[lindex $return_value_overquoted 0]]
    set return_flag 1    
}
    
proc ::completions_menu::do_cancel {tl} {
    variable return_flag 
    variable return_value
    set tbl [path_to_tbl $tl]
    set return_value {}
    set return_flag 1    
}


# Billet only
# Args: items is a list
# Returns: item selected or "" is Esc (Control-w) pressed
proc ::completions_menu::run {items args} {
    named_args $args {-owner {} -title "::completions_menu::run" -width 40}

    # rename to generated toplevel id
    variable return_flag 0
    variable return_value {}

    set tl .completions_menu

    catch {destroy $tl}

    toplevel $tl
    wm title $tl $(-title)

    frame $tl.tf
    set tbl [path_to_tbl $tl]
        
    tablelist::tablelist $tbl -columns [list $(-width) "Invisible label"] -stretch 0 \
        -showlabels 0 \
        -foreground \#000000 \
        -font tkconfixed -borderwidth 1 -highlightthickness 0 \
        -stripebackground \#F3F3F3 -exportselection 0 \
        -width [expr {$(-width)+2}]

    $tbl columnconfigure 0 -wrap true

    set quoted_items [lmap x $items {list $x}]
    $tbl insertlist 0 $quoted_items

    set f1 $tl.tf
    scrollbar $f1.sy -orient v -command [list $tbl yview]
    $tbl configure -yscrollcommand [list $f1.sy set]
    grid $tbl - $f1.sy -sticky news
    grid columnconfigure $f1 0 -weight 1
    grid columnconfigure $f1 1 -weight 1
    grid rowconfigure $f1 0 -weight 1

    pack $f1 -side top -fill both -expand 1

    set esc_binding "after 0 ::completions_menu::do_cancel [list $tl]"

    bind $tl <Escape> "$esc_binding ; break"
    ::clcon_key::b bind $tl <Control-Key-w> "$esc_binding ; break"

    set ok_binding "after 0 ::completions_menu::do_return [list $tl]; break"

    bind $tl <Return> $ok_binding
    bind $tl <Double-1> $ok_binding

    wm protocol $tl WM_DELETE_WINDOW $esc_binding

    ::tablelist_util::GotoIndex $tbl 0 

    focus $tbl
    $tbl see 0
    grab $tl

    set return_value {}

    vwait ::completions_menu::return_flag

    catch {destroy $tl}

    if {$(-owner) ne {}} {
        focus $(-owner)
    }

    return $return_value

}
