#?-Find
#?
#? Enter the "string" you want to search for in the input box "Find what" and then push the "Find next" button.
#? The next occurance of the "string" in the text will be marked, if you want to find the next occurance-
#? push the "Find next" button once more.
#?
#? The match case makes the search case sensitive.
#? The Up and Down radio buttons indicates in which direction the search will go.
#?
#? Note. The search allways starts from where the cursor is positioned.
#?
#? For the Grep function see %LGrep%
#?
#?-Grep
#?
#? Enter the "string" you want to search for in the input box "Find what" and then push the "Grep" button. 
#? The editor will now search for all occurances of "string" in all open files.
#? The result will be presented in a Listbox where you can navigate amongst the files and the occurances of "string"
#?
#?-Replace
#?
#? Enter the "string" you want to search for in the input box "Find what" and
#? enter the "string2" that you wish to replace it with in the "Replace with" input box.
#?  
#? If you push the "Replace" button the editor will replace the next occurance of "string" with "string2".
#? If you push the "Replace all" button the editor will replace all occurrances of "string" with "string2" in the current open file.
#?
#? If the "Confirm replace" checkbox is checked you will be prompted before the editor will replace anything.
#?
#? Related topics: %LFind%

proc greplist { greps } {
    global glb c window

    set glb .glb
    catch { destroy $glb }

    toplevel $glb
    wm title $glb "Grep results"                

    frame $glb.container  -borderwidth 0

    scrollbar $glb.vsb -orient vertical -command [list $glb.listbox yview]
    scrollbar $glb.hsb -orient horizontal -command [list $glb.listbox xview]

    mclistbox::mclistbox $glb.listbox \
        -bd 0 \
        -height 10 \
        -width 60 \
        -columnrelief flat \
        -labelanchor w \
        -columnborderwidth 0 \
        -selectmode extended \
        -labelborderwidth 1 \
        -fillcolumn file \
        -xscrollcommand [list $glb.hsb set] \
        -yscrollcommand [list $glb.vsb set] \
        -background $c(color-editbg) -borderwidth 0



    # add the columns we want to see
    $glb.listbox column add file -label "File"          -width 20
    $glb.listbox column add  text -label "Text"          -width 40

    grid $glb.vsb -in $glb.container -row 0 -column 1 -sticky ns
    grid $glb.hsb -in $glb.container -row 1 -column 0 -sticky ew
    grid $glb.listbox -in $glb.container -row 0 -column 0 -sticky nsew -padx 0 -pady 0
    grid columnconfigure $glb.container 0 -weight 1
    grid columnconfigure $glb.container 1 -weight 0
    grid rowconfigure    $glb.container 0 -weight 1
    grid rowconfigure    $glb.container 1 -weight 0

    pack $glb.container -side top -fill both -expand y

    set i 0
    while {$i<=[llength $greps]} {
        set win [lindex $greps $i]
        incr i
        incr i
        if {$win!=""} {  $glb.listbox insert end "{[win::pton $window($win,name)]} [lindex $greps $i]" }
        incr i
    }

    bind $glb.listbox <Double-Button-1> {
        global glb
        set po [$glb.listbox curselection]
        set win [lindex $greps [expr $po * 3]]
        set pos [lindex $greps [expr $po * 3+1]]
        set str [lindex $greps [expr $po * 3+2]]

        win::activate $win
        .text see $pos
        .text tag delete sel
        tkTextSetCursor .text $pos
        .text tag add sel $pos "$pos + [string length $str] char"
    }

}

proc GrepIt {} {
    global SearchString SearchPos SearchDir findcase current_window window glb greps

    c $SearchString $SearchPos $SearchDir $findcase

    set greps ""

    if {$SearchString!=""} {
        if {$findcase=="1"} {
            set caset "-exact"
	} else {
            set caset "-nocase"
	}
        set limit end

        set cont 1
        set startw $current_window
        set i 0

        while {$cont==1} {
            set SearchPos "1.0"


            set found 1 
            while {$found==1} {

                set leng [string length $SearchString]
                set SearchPos [ .text search $caset -$SearchDir $SearchString $SearchPos $limit]
                if {$SearchPos != ""} {
                    set sta "[lindex [split $SearchPos "."] 0].0"

                    set str [list [string trim [.text get $sta "$sta+1 line"] "\n" ] ]
                    set greps "$greps $current_window $sta {$str} "


                    if {$SearchDir == "forwards"} {
                        set SearchPos "$SearchPos+$leng char"
                    }         
                } else {
                    set found 0
                }
            }

            win::Next
            if {$current_window==$startw} {set cont 0}

        } 

    }
    greplist $greps
}


################################# Find proc #################################

proc FindIt {} {
    global SearchString SearchDir findcase

    set SearchPos insert

    c $SearchString $SearchPos $SearchDir $findcase

    if {$SearchString!=""} {

        if {$findcase=="1"} {
            set caset "-exact"
	} else {
            set caset "-nocase"
	}
	
        if {$SearchDir == "forwards"} {
            set limit end
        } else {
            set limit 1.0
        }

        set leng [string length $SearchString]
        set SearchPos [ .text search $caset -$SearchDir $SearchString $SearchPos $limit]

        if {$SearchPos != ""} {
            .text see $SearchPos

            if {$SearchDir == "forwards"} {
                tkTextSetCursor .text "$SearchPos+$leng char"        
            } else { tkTextSetCursor .text $SearchPos }

            .text tag add sel $SearchPos  "$SearchPos+$leng char"

        } else {
            bgerror "End of document reached"
        }

    }
}



proc question {} {
    global r
    set q .question
    set r "0"
    catch {destroy $q}
    toplevel $q
    wm title $q "Replace ?"
    label $q.label -text "Replace this occurance ?"
    frame $q.buttons
    button $q.buttons.yes -text "Yes" -command "set r 1" 
    button $q.buttons.no  -text "No" -command  "set r 0"
    pack $q.buttons.yes $q.buttons.no -side left
    pack $q.label $q.buttons
    powin $q
    grab $q
    vwait r
    destroy $q
    return $r
}



proc ReplaceIt {n} {
    global SearchString SearchDir ReplaceString findcase window current_window rconfirm

    set SearchPos insert
    c 
    set window($current_window,echange) 1
    set window($current_window,change) 1

    if {$findcase=="1"} {
	set caset "-exact"
    } else {
	set caset "-nocase"
    }
    

    if {$SearchDir == "forwards"} {
        set limit end
    } else {
        set limit 1.0
    }

    set leng [string length $SearchString]
    set SearchPos [ .text search $caset -$SearchDir $SearchString $SearchPos $limit]

    if {$SearchPos != ""} {
        .text see $SearchPos

	if {$rconfirm==1} { 
            .text tag add sel $SearchPos  "$SearchPos+$leng char"
            if {[question]==1} {
                .text delete $SearchPos "$SearchPos+$leng char"
                .text insert $SearchPos $ReplaceString
            }
            .text tag remove sel $SearchPos
        } else {
            .text delete $SearchPos "$SearchPos+$leng char"
            .text insert $SearchPos $ReplaceString
        }
        

	if {$SearchDir == "forwards"} {
            tkTextSetCursor .text "$SearchPos+$leng char"        
	} else { tkTextSetCursor .text $SearchPos }

    } else {
        set SearchPos "0.0"
    }
}

proc ReplaceAll {} {
    global SearchString SearchDir ReplaceString findcase window current_window

    set window($current_window,echange) 1
    set window($current_window,change) 1

    c
    ReplaceIt norec
    while {$SearchPos!="0.0"} {
        ReplaceIt norec
    }
}

proc CancelFind {w} {
    .text tag delete tg1
    destroy $w
}

proc Find {typ} {
    global SearchString SearchDir ReplaceString findcase rconfirm c
    c
    set find .find
    catch {destroy $find}
    toplevel $find
    wm title $find "Find"
    wm resizable $find 0 0

    frame $find.l
    frame $find.l.f
    frame $find.l.f.f1
    label $find.l.f.f1.label -text "Find what:" -width 11  
    xentry $find.l.f.f1.entry  -textvariable SearchString -width 30 
    pack $find.l.f.f1.label $find.l.f.f1.entry -side left

    $find.l.f.f1.entry selection range 0 end

    if {$typ=="replace"} {
        frame $find.l.f.f2
        label $find.l.f.f2.label2 -text "Replace with:" -width 11
        xentry $find.l.f.f2.entry2  -textvariable ReplaceString -width 30
        pack $find.l.f.f2.label2 $find.l.f.f2.entry2 -side left

        pack $find.l.f.f1 $find.l.f.f2 -side top
        bind $find.l.f.f2.entry2 <Return> "ReplaceIt -" 
        bind $find.l.f.f2.entry2 <Alt-R> "ReplaceIt -"
        bind $find.l.f.f2.entry2 <Alt-r> "ReplaceIt -"
        bind $find.l.f.f2.entry2 <Alt-A> "ReplaceAll -"
        bind $find.l.f.f2.entry2 <Alt-a> "ReplaceAll -"
        bind $find.l.f.f1.entry <Alt-R> "ReplaceIt -"
        bind $find.l.f.f1.entry <Alt-r> "ReplaceIt -"
        bind $find.l.f.f1.entry <Alt-A> "ReplaceAll -"
        bind $find.l.f.f1.entry <Alt-a> "ReplaceAll -"
    } else {
        pack $find.l.f.f1
        bind $find.l.f.f1.entry <Return> FindIt 
        bind $find.l.f.f1.entry <Alt-N> FindIt 
        bind $find.l.f.f1.entry <Alt-n> FindIt 
    }


    frame $find.f2
    xbutton $find.f2.button1 -text "Find Next" -command "FindIt" -width 10 -height 1 -underline 5
    xbutton $find.f2.button9 -text "Grep" -command "destroy $find; GrepIt"  -width 10 -underline 0
    xbutton $find.f2.button2 -text "Cancel" -command "CancelFind $find" -width 10 -underline 0

    if {$typ=="replace"} {
        xbutton $find.f2.button3 -text "Replace" -command "ReplaceIt -" -width 10 -height 1 -underline 0
        xbutton $find.f2.button4 -text "Replace All" -command ReplaceAll -width 10 -height 1 -underline 8
        pack $find.f2.button3 $find.f2.button4 $find.f2.button2  -pady 4
    } else {
        pack $find.f2.button1 $find.f2.button9 $find.f2.button2  -pady 4
    }


    frame $find.l.f4
    frame $find.l.f4.f3 -borderwidth 2 -relief groove
    radiobutton $find.l.f4.f3.up -text "Up" -underline 0 -variable SearchDir -value "backwards" 
    radiobutton $find.l.f4.f3.down -text "Down"  -underline 0  -variable SearchDir -value "forwards" 
    pack $find.l.f4.f3.up $find.l.f4.f3.down -side left 


    if {$typ=="replace"} {
        frame $find.l.f4.f
        set rconfirm 1
        checkbutton $find.l.f4.f.cbox1 -text "Match case" -variable findcase -underline 0
        checkbutton $find.l.f4.f.cbox2 -text "Confirm replace"  -variable rconfirm -underline 0
        pack $find.l.f4.f.cbox1 $find.l.f4.f.cbox2 -side top -padx 0 -fill x
        pack $find.l.f4.f $find.l.f4.f3 -side left -padx 10
    } else {

        checkbutton $find.l.f4.cbox1 -text "Match case" -variable findcase -underline 0
        pack $find.l.f4.cbox1 $find.l.f4.f3 -side left -padx 10
    }


    pack $find.l.f
    pack $find.l.f4 -pady 11
    pack $find.l $find.f2 -side left -padx 1

    bind $find <Escape> "destroy $find"
    focus $find.l.f.f1.entry
    grab $find

    powin $find
}

### Proc called by "Search/Goto line" menu
proc Goto_line_ask {} {
    global c

    set gow .goto_line
    catch {destroy $gow}
    toplevel $gow
    wm title $gow "Where ?"

    frame $gow.fr1
    frame $gow.fr2
    pack $gow.fr1 $gow.fr2 -side left

    label $gow.fr1.lab -text "Goto line number:"
    xentry $gow.fr1.ent 
    pack $gow.fr1.lab $gow.fr1.ent -side left -pady 10 -padx 10

    xbutton $gow.fr2.ok -text Ok -width 10 -command "Goto_line $gow"
    xbutton $gow.fr2.can -text Cancel -width 10 -command "focus .text;destroy $gow"
    pack $gow.fr2.ok $gow.fr2.can -side top -padx 10 -pady 5

    focus $gow.fr1.ent

    bind $gow.fr1.ent <Return> "Goto_line $gow"
    bind $gow.fr1.ent <KP_Enter> "Goto_line $gow"
    bind $gow.fr1.ent <Escape> "focus .text ; destroy $gow"
    powin $gow
    grab $gow
}                                          


proc Goto_line {w} {
    set value [$w.fr1.ent get]
    if {[catch {expr $value} err]} {
        return }
    tkTextSetCursor .text $value.0
    win::updateln
    focus .text
    destroy $w
}       





