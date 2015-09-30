namespace eval SNotebook {

    # create scroll buttons  
    foreach anchor {n s e w} arrow {uparrow downarrow leftarrow rightarrow} {
        set uanchor [string toupper $anchor]
        ttk::style layout ${uanchor}Button.TButton [list Button.focus -sticky nswe -children [list ${uanchor}Button.$arrow -sticky nswe]]
    }
    
    variable scrollpos
    variable hsizes
    variable tabheight
    
    proc _updatehsizes {nb} {
        if {![winfo exists $nb.escroll]} {
            ttk::button $nb.escroll -style EButton.TButton -command [string map [list \$nb $nb] {
                if {$::SNotebook::scrollpos($nb)>0} {
                    incr ::SNotebook::scrollpos($nb) -1
                    ::SNotebook::_scroll_notebook $nb
                }
            }]
        }
        if {![winfo exists $nb.wscroll]} {
            ttk::button $nb.wscroll -style WButton.TButton -command [string map [list \$nb $nb] {
                if {$::SNotebook::scrollpos($nb)+1<[llength [$nb tabs]]} {
                    incr ::SNotebook::scrollpos($nb) 
                    ::SNotebook::_scroll_notebook $nb
                }
            }]
        }
        variable hsizes
        variable tabheight
        set ntabs [$nb index end]
        set tabs [$nb tabs] 
        ttk::notebook .tmp -style [$nb cget -style]
        .tmp add [frame .tmp.rightarrow] -text ">>>>>" -state normal
        update idletasks
        set p0 [winfo reqwidth .tmp]
        set hsizes($nb) $p0
        for {set i 0} {$i<$ntabs} {incr i} {
            set tab [lindex $tabs $i]
            if {1} { ;# theme-independent, but could have side effects 
                eval .tmp add [ttk::frame .tmp.f$i] [::SNotebook::$nb tab $tab] -state normal
                update idletasks
                set hsizes($tab) [expr [winfo reqwidth .tmp]-$p0]
                set p0 [winfo reqwidth .tmp]
            } else { ;# there is a theme-dependable constant, which includes padding and tab margins
                eval ttk::label .tmp2 -style TNotebook.Tab [dict remove [::SNotebook::$nb tab $tab] -sticky] 
                set hsizes($tab) [expr [winfo reqwidth .tmp2]+10]
                destroy .tmp2
            }
        }
        destroy .tmp
        ttk::label .tmp -style TNotebook.Tab
        set tabheight [winfo reqheight .tmp]
        destroy .tmp
    }
    
    

    proc _scroll_notebook {nb} { ;# assumes tabposition is nw or ne
        variable scrollpos
        variable hsizes 
        variable tabheight
        if {![info exists scrollpos($nb)]} {
            set scrollpos($nb) 0
        }
        set startindex $scrollpos($nb)
        for {set i 0} {$i<$startindex} {incr i} {
            ::SNotebook::$nb hide $i
        }
        set ntabs [$nb index end]
        set tabs [$nb tabs] 
        
        set availw [winfo width $nb]
        set reqw $hsizes($nb)
        set overflow 0
        for {} {$i<$ntabs} {incr i} {
            set tab [lindex $tabs $i]
            incr reqw $hsizes($tab)
            ::SNotebook::$nb add $tab
            if {$reqw>$availw} {
                incr i
                set overflow 1
                break
            }
        }
        for {set j $i} {$j<$ntabs} {incr j} {
            ::SNotebook::$nb hide [lindex $tabs $j]
        }
        ::SNotebook::$nb add [lindex $tabs [expr {$ntabs-1}]]
        set h $tabheight
        
        set eh [expr 4*$h/5]
        if {$startindex>0} {
            set ew [expr $eh*[winfo reqwidth $nb.escroll]/[winfo reqheight $nb.escroll]]
            place $nb.escroll -x 0 -y 0 -width $ew -height $eh
        } else {
            place forget $nb.escroll
        }
        if {$overflow} {
            set ew [expr $eh*[winfo reqwidth $nb.wscroll]/[winfo reqheight $nb.wscroll]]
            place $nb.wscroll -relx 1.0 -x -$ew -y 0 -width $ew -height $eh
        } else {
            place forget $nb.wscroll
        }
    }
    
    proc snotebook {path args} {
        ttk::notebook $path {*}$args
        _updatehsizes $path
        bindtags $path [linsert [bindtags $path] 1 SNotebook]
        bind SNotebook <Configure> {::SNotebook::_scroll_notebook %W}
        rename ::$path ::SNotebook::$path
        proc ::$path {cmd args} [string map [list \$path $path] {
            switch $cmd {
                index -
                configure -
                cget -
                identify -
                instate -
                select -
                state -
                tabs {
                    ::SNotebook::$path $cmd {*}$args
                }
                default {
                    bind SNotebook <Configure> {}
                    set res [::SNotebook::$path $cmd {*}$args]
                    ::SNotebook::_updatehsizes $path
                    bind SNotebook <Configure> {::SNotebook::_scroll_notebook %W}
                    ::SNotebook::_scroll_notebook $path
                    return $res
                }
            }
        }]
        return $path
    }

}

::SNotebook::snotebook .nb
pack .nb -fill both -expand true

set i 0
foreach theme [ttk::themes] {
    frame .f$i
    pack [button .f$i.b -text "Theme: $theme" -command "ttk::setTheme $theme"] -anchor nw
    .nb add .f$i -text "Tab $i"
    incr i
}
for {} {$i<20} {incr i} {
    .nb add [text .t$i] -text "Tab $i"
}
.nb add [text .gotoright] -text ">"

set PreviousIndex 0

bind .nb <<NotebookTabChanged>> {
    global PreviousIndex 
    if {[.nb index current] == 20} {
        ::SNotebook::_scroll_notebook .nb

        Здесь надо прокрутить и поставить предыдущий индекс в новое значение
    } 
}
