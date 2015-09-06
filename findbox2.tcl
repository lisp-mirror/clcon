# Consider this as a part of findreplace.tcl
# area is a widget to operate on. Currently text or tablelist,
# type of widget is specified by AreaType, can be "text" or "tablelist"
# FndOrRepl can be "replace" or... not "replace", say, "find"
# EnsurePopulatedCmd is relevant for AreaType eq "tablelist" only
proc ::fndrpl::OpenFindBox {area AreaType FndOrRpl EnsurePopulatedCmd} {
    variable SearchString
    variable SearchDir
    variable ReplaceString
    variable findcase
    variable rconfirm
    variable TreeSearchState

    unset -nocomplain TreeSearchState
    
    c
    set find $area.find
    catch {destroy $find}
    toplevel $find
    wm title $find "Find"
    wm resizable $find 0 0

    frame $find.l
    frame $find.l.f
    frame $find.l.f.f1
    label $find.l.f.f1.label -text "1.Find what:" -width 11 -underline 0
    entry $find.l.f.f1.entry  -textvariable ::fndrpl::SearchString -width 30 
    pack $find.l.f.f1.label $find.l.f.f1.entry -side left

    $find.l.f.f1.entry selection range 0 end

    if {$FndOrRpl=="replace"} {
        if {$AreaType=="text"} {
            # later make similar to "find" case.
        } else {
            error "Unsupported AreaType $AreaType"
        }
    } elseif {$FndOrRpl=="find"} {
        if {$AreaType=="text"} {
            set SearchCmd "::fndrpl::FindIt $area"
        } elseif {$AreaType=="tablelist"} {
            set SearchCmd "::fndrpl::TreeSearchTextOuter $area $EnsurePopulatedCmd \$tmpContinueP"
        } else {
            error "Unknown $AreaType"
        }
    } else {
        error "Wrong FndOrRpl $FndOrRpl"
    }


    if {$FndOrRpl=="replace"} {
        frame $find.l.f.f2
        label $find.l.f.f2.label2 -text "Replace with:" -width 11
        entry $find.l.f.f2.entry2 -textvariable ::fndrpl::ReplaceString -width 30
        pack $find.l.f.f2.label2 $find.l.f.f2.entry2 -side left

        pack $find.l.f.f1 $find.l.f.f2 -side top
        bind $find.l.f.f2.entry2 <Return> "::fndrpl::ReplaceIt $area -" 
    } elseif {$FndOrRpl=="find"} {
        pack $find.l.f.f1
        bind $find <Return> "set tmpContinueP 0; $SearchCmd"
        bind $find <F3>     "set tmpContinueP 1; $SearchCmd"
    }
    
    frame $find.f2
    button $find.f2.button1 -text "Find Next" -command "set tmpContinueP {}; $SearchCmd" -width 10 -height 1 
    
    button $find.f2.button9 -text "Find allwindows" -command "destroy $find; ::fndrpl::GrepIt $area"  -width 10 -underline 0 -state disabled

    # Cancel is disabled due to issue 41
    button $find.f2.button2 -text "Cancel" -command "::fndrpl::CancelFind $area $find" -width 10 -underline 0 -state disabled

    if {$FndOrRpl=="replace"} {
        button $find.f2.button3 -text "Replace" -command "::fndrpl::ReplaceIt $area -" -width 10 -height 1 -underline 0
        button $find.f2.button4 -text "Replace All" -command "::fndrpl::ReplaceAll $area" -width 10 -height 1 -underline 8
        pack $find.f2.button3 $find.f2.button4 $find.f2.button2  -pady 4
    } else {
        pack $find.f2.button1 $find.f2.button9 $find.f2.button2  -pady 4
    }


    frame $find.l.f4
    frame $find.l.f4.f3 -borderwidth 2 -relief groove
    radiobutton $find.l.f4.f3.up -text "2.Up" -underline 0 -variable ::fndrpl::SearchDir -value "backwards" 
    radiobutton $find.l.f4.f3.down -text "Down"  -variable ::fndrpl::SearchDir -value "forwards" 
    pack $find.l.f4.f3.up $find.l.f4.f3.down -side left 


    if {$FndOrRpl=="replace"} {
        frame $find.l.f4.f
        set rconfirm 1
        checkbutton $find.l.f4.f.cbox1 -text "3.Match case" -variable ::fndrpl::findcase -underline 0
        checkbutton $find.l.f4.f.cbox2 -text "Confirm replace"  -variable ::fndrpl::rconfirm -underline 0
        pack $find.l.f4.f.cbox1 $find.l.f4.f.cbox2 -side top -padx 0 -fill x
        pack $find.l.f4.f $find.l.f4.f3 -side left -padx 10
    } else {

        checkbutton $find.l.f4.cbox1 -text "3.Match case" -variable ::fndrpl::findcase -underline 0
        pack $find.l.f4.f3 -side left -padx 10
        pack $find.l.f4.cbox1 -side left -padx 10
    }

    if {$FndOrRpl=="replace"} {
        puts "Bindings for alt-letter are not created for Replace"
    } else {
        bind $find <Alt-Key-1> "focus $find.l.f.f1.entry"
        bind $find <Alt-Key-2> "focus $find.l.f4.f3.up"
        bind $find <Alt-Key-3> "focus $find.l.f4.cbox1"
    }

    pack $find.l.f
    pack $find.l.f4 -pady 11
    pack $find.l $find.f2 -side left -padx 1

    bind $find <Escape> "destroy $find"
    focus $find.l.f.f1.entry
    grab $find

    powin $find $area
}

        
    
    
    
    
                             
