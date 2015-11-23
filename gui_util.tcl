package require Tk

namespace eval ::gui_util {

    # clcon_text_options is a list
    proc frame_clcon_text_and_scrollbars {frame_pathName clcon_text_options} {
        set w $frame_pathName
        frame $w
        ::clcon_text::clcon_text $w.text
        $w.text configurelist $clcon_text_options
        scrollbar $w.sx -orient h -command [list $w.text xview]
        scrollbar $w.sy -orient v -command [list $w.text yview]
        ConfigureTextFonts $w.text
        $w.text configure \
            -xscrollcommand [list $w.sx set] \
            -yscrollcommand [list $w.sy set]
        
        # now layout elements in frame
        grid $w.text - $w.sy -sticky news
        grid $w.sx - -sticky ew
        grid columnconfigure $w 0 -weight 1
        grid columnconfigure $w 1 -weight 1
        grid rowconfigure $w 0 -weight 1

        # Don't forget to layout frame itself!
        return $w
    }

    # clcon_text with vertical scrollbar. options are in the list
    proc frame_clcon_text_and_vertical_scrollbar {frame_pathName clcon_text_options} {
        set w $frame_pathName
        frame $w
        set text $w.text
        ::clcon_text::clcon_text $text -readonly 1

        ::gui_util::ConfigureTextFonts $text
        
        $text configure -yscrollcommand [list $w.sy set] 
        scrollbar $w.sy -orient v -command [list $text yview]
        
        grid $text - $w.sy -sticky news
        grid columnconfigure $w 0 -weight 1 
        grid columnconfigure $w 1 -weight 1
        grid rowconfigure $w 0 -weight 1
    }
    

    # Sets some sensible fonts for text widget
    proc ConfigureTextFonts {text} {
        variable ::tkcon::COLOR
        $text configure \
            -foreground $COLOR(stdin) \
            -background $COLOR(bg) \
            -insertbackground $COLOR(cursor) \
            -font $::tkcon::OPT(font) -borderwidth 1 -highlightthickness 0 \
            -undo 1
    }

    proc ClearMenu {m} {
        $m delete 0 end
    }

    # I don't know if someone needs it. Grep it FIXME
    proc WidgetParent { w } {
        set lst [split $w .]
        set sublist [lrange $lst 0 end-1]
        return [join $sublist .]
    }

    # Returns list of lists three elements: bindtag event script
    proc ListAllWindowBindings { w } {
        set result {}
        foreach bindtag [bindtags $w] {
            foreach binding [bind $bindtag] {
                lappend result \
                    [list $bindtag $binding \
                         [bind $bindtag $binding]]
            }
        }
        return $result
    }

    # Adds putd (entry) to each binding
    # Example:  ::gui_util::InstrumentAllBindingsWithPutd [::edt::c_btext]
    # ::gui_util::InstrumentAllBindingsWithPutd [::edt::c_text] 454570000
    # As binding fires, you have string like InstrumentAllBindingsWithPutd/454560029 in your putd log.
    # Then call .. ::gui_util::ListAllWindowsBindings [::edt::c_btext] and find number there with console Find box. 
    proc InstrumentAllBindingsWithPutd { w {InitialNumber 454560000}} {
        foreach entry [ListAllWindowBindings $w] {
            foreach { bindtag event script } $entry {}
            set script [string cat "putd InstrumentAllBindingsWithPutd/[incr InitialNumber];" $script]
            bind $bindtag $event $script
        }
        return $InitialNumber
    }

    
    # # To be bound on configure event of toplevel window $w as
    # #  bind . <Configure> "gui_util::RememberPositionOnConfigureEvent $w %W %x %y %h %w"
    # # so that it records window position and size into a variable
    # # later use wm geometry $toplevel $thatvar to restore window geometry
    # ONE PROBLEM - DOES NOT WORK
    # Just not destroy window to fix the problem
    # proc RememberPositionOnConfigureEvent { target w x y height width varname } {
    #     variable $varname
    #     upvar \#0 $varname store
    #     if { $w eq $target && [wm state $w] eq "normal" } {
    #         set store [join [list $width x $height + $x + $y] ""]
    #     }
    # }
        

    # 
    # proc NoteWidgetsChanged {widget suffix_list proc_body} {
    # }
    # See example in FindBox

    proc FocusWindowByName {window {widget {}}} {
        if {$window eq {}} {
            bell
            return
        } 
        set code [catch {
            set state_now [wm state $window]
            if {[lsearch {iconic withdrawn} $state_now] >= 0} {
                wm deiconify $window
            }
            raise $window
            if {$widget ne {}} {
                focus $widget
            } else {
                focus $window
            }
        }]
        if {$code} {
            bell
        }
        return 
    } 
}

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
