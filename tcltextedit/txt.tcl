namespace eval  txt { 

    namespace export keypress tagall copy paste copycut deleteline texteval deleol deleow dupline searchsel cut backspace copyfrom_clip u


    proc keypress {A K} {
        global window current_window
        set list_key {Alt_L Mode_switch Control_L Control_R Shift_R Shift_L Up Down Right Left End Home Insert Prior Next}
        if { [lsearch -exact $list_key $K] == -1} {
            set window($current_window,change) 1
            if {$window($current_window,echange)==0} {
		set window($current_window,echange) 1
		win::update
            }
	}
        win::updateln
        if {$A==""} { macro::rec "macro::key" $K } else { macro::rec "macro::key" $A }
    }

    proc copyfrom_clip {w} {
        global ftypes window current_window
        c
        clipboard clear -displayof .text
        set file [tk_getOpenFile -title "Load clip" -filetypes $ftypes]
        if {$file != ""} {
            set f [open $file r]
            while {![eof $f]} {
                clipboard append -displayof .text [read $f 10000]
            }
            close $f
	}
    }

    proc undo { w } {
        c
        macro::rec txt::undo $w
        $w undo
    }

    proc tagall { w } {
        c
        macro::rec txt:tagall $w
        .text tag add sel 1.0 end
    }

    proc copy { w } {
        c
        macro::rec txt::copy $w
        tk_textCopy $w
    }

    proc paste { w } {
        global window current_window
        c
        set window($current_window,change) 1
        macro::rec txt::paste $w
        tk_textPaste $w
    }

    proc copycut { w } {
        global window current_window
        c
        set window($current_window,change) 1
        macro::rec txt::copycut $w
        tk_textCopy $w
        tk_textCut $w
    }

    proc deleteline { w } {
        global window current_window
        c
        set window($current_window,change) 1
        macro::rec deleteline $w
        $w delete {insert linestart} {insert lineend}
        if {! [$w compare "insert+1 char" == end] } {
            $w delete insert "insert+1 char"
	}
    }

    proc texteval {w} {
        global window current_window
        c

        macro::rec texteval $w

        if {[$w tag nextrange sel 1.0 end]!=""} {
            set exp [$w get sel.first sel.last]
            set exp [expr $exp]
            $w mark set kaka sel.first
            $w delete sel.first sel.last
            $w insert kaka $exp
            $w mark unset kaka
            set window($current_window,change) 1
        } else { bgerror "You must select some numbers first.\n See Evaluate for more information." }
    }

    proc deleol { w } {
        global window current_window
        c
        set window($current_window,change) 1
        macro::rec deleol $w
        $w delete {insert} {insert wordend}
    }

    proc deleow { w } {
        global window current_window
        c
        set window($current_window,change) 1
        macro::rec deleow $w
        $w delete {insert} {insert wordend}
    }

    proc dupline { w } {
        global window current_window
        c
        set window($current_window,change) 1
        macro::rec dupline $w
        set s "[$w get {insert linestart} {insert lineend} ]\n"
        set po {insert linestart}
        $w insert "$po+1 line"  $s
    }

    proc searchsel { w } {
        global SearchString SearchPos SearchDir findcase
        c
        macro::rec searchsel $w
        set SearchString [$w get sel.first sel.last]
        set SearchPos {insert wordend}
        set findcase 0
        set SearchDir "forwards"
        FindIt
    }

    proc cut {w} {
        global window current_window
        c
        set window($current_window,change) 1
        if {[$w tag nextrange sel 1.0 end] != ""} {
            $w delete sel.first sel.last
        } else {
            $w delete insert
            $w see insert
        }    
    }


    proc backspace {w} {
        global window current_window
        c
        set window($current_window,change) 1
        if {[$w tag nextrange sel 1.0 end] != ""} {
            $w delete sel.first sel.last
        } elseif [$w compare insert != 1.0] {
            $w delete insert-1c
            $w see insert
        }
    }

}

