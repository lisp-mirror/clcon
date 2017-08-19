
namespace eval ::fndrpl {
    # Consider this as a part of findreplace.tcl
    # area is a widget to operate on. Currently text or tablelist,
    # type of widget is specified by AreaType, can be "text" or "tablelist"
    # FndOrRepl can be "replace" or... not "replace", say, "find"
    # EnsurePopulatedCmd is relevant for AreaType eq "tablelist" only

    proc OpenFindBox {area AreaType FndOrRpl EnsurePopulatedCmd} {
        variable SearchString
        variable SearchDir
        variable ReplaceString
        variable findcase
        variable rconfirm

        variable SearchParamsChanged

        set SearchParamsChanged 1

        set find $area.find
        catch {destroy $find}
        toplevel $find
        wm title $find "Find"
        wm resizable $find 0 0

        frame $find.l
        frame $find.l.f
        frame $find.l.f.f1
        label $find.l.f.f1.label -text "1.Найти:" -width 14 -underline 0
        entry $find.l.f.f1.entry  -textvariable ::fndrpl::SearchString -width 50 
        pack $find.l.f.f1.label $find.l.f.f1.entry -side left

        $find.l.f.f1.entry selection range 0 end

        if {$FndOrRpl=="replace"} {
            if {$AreaType=="text"} {
                set SearchCmd "::fndrpl::ReplaceIt $area"
                # later make similar to "find" case.
            } else {
                error "Unsupported AreaType $AreaType"
            }
        } elseif {$FndOrRpl=="find"} {
            if {$AreaType=="text"} {
                set SearchCmd "::fndrpl::FindIt $area"
            } elseif {$AreaType=="tablelist"} {
                set SearchCmd [list ::fndrpl::TreeSearchTextOuter $area $EnsurePopulatedCmd]
            } else {
                error "Unknown $AreaType"
            }
        } else {
            error "Wrong FndOrRpl $FndOrRpl"
        }


        if {$FndOrRpl=="replace"} {
            frame $find.l.f.f2
            label $find.l.f.f2.label2 -text "Заменить на:" -width 14
            entry $find.l.f.f2.entry2 -textvariable ::fndrpl::ReplaceString -width 50
            pack $find.l.f.f2.label2 $find.l.f.f2.entry2 -side left

            pack $find.l.f.f1 $find.l.f.f2 -side top
            bind $find.l.f.f2.entry2 <Return> "::fndrpl::ReplaceIt $area" 
        } elseif {$FndOrRpl=="find"} {
            pack $find.l.f.f1
            # Is there a sense to have two distinct keys for the same command? 
            bind $find <Return> "$SearchCmd"
            bind $find <F3>     "$SearchCmd"
        }
        
        frame $find.f2
        button $find.f2.button1 -text "Найти дальше" -command "$SearchCmd" -width 10 -height 1 
        
        # Cancel is disabled due to issue 41
        button $find.f2.button2 -text "Отмена" -command "::fndrpl::CancelFind $area $find" -width 10 -underline 0 -state disabled

        if {$FndOrRpl=="replace"} {
            set cmd "::fndrpl::ReplaceIt $area"
            button $find.f2.button3 -text "Заменить" -command $cmd -width 14 -height 1 

            set cmd "::fndrpl::ReplaceAll $area"
            button $find.f2.button4 -text "6.Заменить все" -command $cmd -width 14 -height 1 -underline 0
            ::clcon_key::b bind $find <Alt-Key-6>  $cmd
            
            pack $find.f2.button3 $find.f2.button4 $find.f2.button2  -pady 4
        } else {
            pack $find.f2.button1 $find.f2.button2  -pady 4
        }

        # $find.l.f4 contains: up and down group box

        frame $find.l.f4
        frame $find.l.f4.f3 -borderwidth 2 -relief groove
        radiobutton $find.l.f4.f3.down -text "Вперёд (к концу док-та)"  -variable ::fndrpl::SearchDir -value "forwards" 
        radiobutton $find.l.f4.f3.up -text "2.Назад (к началу)" -underline 0 -variable ::fndrpl::SearchDir -value "backwards" 
        pack $find.l.f4.f3.up -side top -anchor w
        pack $find.l.f4.f3.down -side bottom -anchor w


        frame $find.l.f4.f
        checkbutton $find.l.f4.f.cbox1 -text "3.Учитывать регистр" -variable ::fndrpl::findcase -underline 0
        pack $find.l.f4.f.cbox1 -side left -padx 0 -fill x

        if {$FndOrRpl=="replace"} {
            set rconfirm 1
            checkbutton $find.l.f4.f.cbox2 -text "Подтверждать замену"  -variable ::fndrpl::rconfirm -underline 0
            pack $find.l.f4.f.cbox2 -side right -padx 0 -fill x
        }

        pack $find.l.f4.f $find.l.f4.f3 -side left -padx 10

        if {$AreaType ne "tablelist"} {
            frame $find.l.f4.policyBox -borderwidth 2 -relief groove
            radiobutton $find.l.f4.policyBox.any_bounds -text "5.Текст" -underline 0 -variable ::fndrpl::BoundaryPolicy -value "any_bounds" 
            radiobutton $find.l.f4.policyBox.lisp_identifier -text "Сивмол Лиспа" -variable ::fndrpl::BoundaryPolicy -value "lisp_identifier" 
        
            pack $find.l.f4.policyBox.any_bounds -side top -anchor w
            pack $find.l.f4.policyBox.lisp_identifier -side bottom -anchor w
            pack $find.l.f4.policyBox -side left -padx 10
        }

        if {$AreaType ne "tablelist"} {
            bind $find <Alt-Key-5> "focus $find.l.f4.policyBox.any_bounds"
        }

        bind $find <Alt-Key-2> "focus $find.l.f4.f3.up"
        bind $find <Alt-Key-3> "focus $find.l.f4.f.cbox1"

        if {$FndOrRpl=="replace"} {
        } else {
            bind $find <Alt-Key-1> "focus $find.l.f.f1.entry"
        }

        pack $find.l.f
        pack $find.l.f4 -pady 11
        pack $find.l $find.f2 -side left -padx 1

        bind $find <Escape> "destroy $find"
        focus $find.l.f.f1.entry


        grab $find

        ::win_lay::PositionATool $find
        # powin $find $area
    }

}        
    
    
    
    
                             
