## Buffer-list
## Design for the future: we will have several windows, but every buffer is
## bound to only one window. Sometimes we might try to use peering text widgets
## to clone some buffers to several windows, which is useful sometimes.

proc ::clconcmd::f {} {
    tkcon main ::buli::BufferListBox 
}


namespace eval ::buli {
    variable data
    variable tv
    variable TitleListWindow

    catch {font create tkconfixed -family Courier -size -20}

    proc InitData {} {
        variable data
        set data {}
    }

    # title -> key , DetailsCode -> w
    # There is a design problem. We mix visual and non-visual
    # activity while we don't know if visual component exists
    # We recreate non-visual data at creation of visual control
    # That seem to be a bad design. On the other hand, we would like
    # to be able to change contents of visual component incrementally
    # This was desired for error-browser and this can also be desired
    # for buffer-list. E.g. we would like to remove deleted window
    # without moving keyboard focus on the list. 
    proc AppendData {title w} {
        variable TitleListWindow
        variable tv
        variable data

        if {!([info exists TitleListWindow]&&[winfo exists $TitleListWindow])} {
            # There maybe no any Buffer List browser. Lets get out!
            return
        }

        set NewItem [dict create title $title w $w]
        lappend data $NewItem

        set tbl $TitleListWindow.tf.tbl    
        $tbl insert end [list $title]

        # If we inserted first item, highlight it
        if {[llength $data] == 1} {
            after idle "$tbl activate 0; $tbl selection set 0 0"
            # ; $tbl selection anchor 0"
            # after idle [$tbl activate 0]
        }
    }

    proc FillData {} {
        variable ::edt::EditorMRUWinList
        foreach p $EditorMRUWinList {
            foreach {key w} $p break
            AppendData $key $w
        }
    }

    proc RefreshData {} {
        variable TitleListWindow
        if {[info exists TitleListWindow]&&[winfo exists $TitleListWindow]} {
            ClearTitleList
        }
        InitData
        FillData
    }
        

    proc CellCmd {row action} {
        variable ::edt::EditorMRUWinList
        variable TitleListWindow
        set p [lindex $EditorMRUWinList $row]
        foreach {key w} $p break
        switch -exact $action {
            ShowBuffer {
                ::edt::ShowExistingBuffer $w
            }
            HideListAndShowBuffer {
                wm withdraw $TitleListWindow
                ::edt::ShowExistingBuffer $w
            }
            CloseBuffer {
                ::edt::EditCloseFile $w $w
            }
            default {
                error "Unknown CellCmd"
            }
        }
    }

    proc MouseCellCmd {w x y action} {
        foreach {tbl x y} [tablelist::convEventFields $w $x $y] break
        set row [$tbl containing $y]
        CellCmd $row $action
    }

    proc KbdCellCmd {w x y action} {
        foreach {tbl x y} [tablelist::convEventFields $w $x $y] break
        set row [$tbl index active]
        CellCmd $row $action
    }
    
    proc ShowSelectedBuffer {tbl HideBufferList} {
        tk_messageBox -message "ShowSelectedBuffer $tbl [$tbl index active]"        
    }
    

    # This is a contiuation assigned on reply on initialization request 
    proc BufferListBox { } {
        # EventAsList is ignored
        variable tv

        InitData 
        
        set w [PrepareGui1]

        set tbl $w.tf.tbl 

        set bodytag [$w.tf.tbl bodytag]
        
        # wcb::callback $tbl before activate ::buli::DoOnSelect
        bind $bodytag <space> {::buli::KbdCellCmd %W %x %y ShowBuffer; break}
        bind $bodytag <Return> {::buli::KbdCellCmd %W %x %y HideListAndShowBuffer; break}
        bind $bodytag <Delete> {::buli::KbdCellCmd %W %x %y CloseBuffer; break}
        bind $bodytag <Double-Button-1> {::buli::MouseCellCmd %W %x %y HideListAndShowBuffer; break}
        
        #    bind $w.tf.tbl <<TablelistCellUpdated>> [list DoOnSelect $w.tf.tbl]
        #    bind $w.tf.tbl <<ListBoxSelect>> [list DoOnSelect $w.tf.tbl]

        FillData
        
        DoGoToTop $w
        
        focus $tbl
        return
        
    }

    proc DoGoToTop {w} {
        wm deiconify $w
        raise $w
    }

    # Fills browser from data 
    proc FillHeaders {tbl} {
        variable data
        foreach item $data {
            set title [dict get $item {title}]
            $tbl insert end [list $title]
        }
    }

    proc TitleListFileMenu {w menu} {
         set m [menu [::tkcon::MenuButton $menu "1.File" file]]
         $m add command -label "1.Close" -underline 0 -accel "Escape" -command [list destroy $w]
        bind $w <Escape>		[list destroy $w]
    }


    proc CellCmdForActiveCell {tbl Cmd} {
        CellCmd [$tbl index active] $Cmd
    }

    proc TitleListBufferMenu {w menu} {
        set m [menu [::tkcon::MenuButton $menu "2.Buffer" buffer]]
        
        set ActivateCmd "::buli::CellCmdForActiveCell $w.tf.tbl HideListAndShowBuffer"
        $m add command -label "Activate" -accel "Return" -command $ActivateCmd

        set CloseCmd "::buli::CellCmdForActiveCell $w.tf.tbl CloseBuffer"
        $m add command -label "Close buffer or file" -accel "Delete" -command $CloseCmd
    }


    proc ClearTitleList {} {
        variable TitleListWindow
        if {[winfo exists $TitleListWindow]} {
            set tbl [::buli::GetTitleListMenuTbl $TitleListWindow]
            $tbl delete 0 end
        }
    }

        
    # Make toplevel widget and its children 
    proc PrepareGui1 {} {
        variable TitleListWindow

        # ---------------------------- make toplevel window TitleListWindow -----------    
        variable ::tkcon::PRIV
        # Create unique edit window toplevel
        set w $PRIV(base).buliTlv
        puts $w
        if {[winfo exists $w]} {
            ClearTitleList
            return $w
        }

        toplevel $w
        wm withdraw $w
        
        # title 
        set word "Buffer list $w"
        wm title $w $word

        set TitleListWindow $w

        # ----------------------------------- menu -------------------
        
        set menu [menu $w.mbar]
        $w configure -menu $menu
        
        TitleListFileMenu $w $menu
        TitleListBufferMenu $w $menu

        
        # --------------------------------- frames-----------------              

        frame $w.tf
        set tbl $w.tf.tbl
        
        tablelist::tablelist $tbl -columns {20 ""} -stretch all -spacing 10
        #$tbl resetsortinfo 

        $tbl configure \
            -foreground \#000000 \
            -font tkconfixed -borderwidth 1 -highlightthickness 1 
        

        $tbl columnconfigure 0 -wrap true  
        
        set f1 $w.tf
        scrollbar $f1.sy -orient v -command [list $tbl yview]
        $tbl configure -yscrollcommand [list $f1.sy set]
        grid $tbl - $f1.sy -sticky news
        grid columnconfigure $f1 0 -weight 1
        grid columnconfigure $f1 1 -weight 1
        grid rowconfigure $f1 0 -weight 1

        pack $f1 -side top -fill x

        $tbl selection anchor 0

        return $w    
    }

    # Returns tablelist by main error browser window
    proc GetTitleListMenuTbl {w} {
        return $w.tf.tbl
    }

    proc TitleOfErrorBrowser {w} {
        set f1 $w.pane.title
        return $w.pane.title.text
    }

    proc DebugStartup {} {
        BufferListBox 
    }

}

# ::buli::DebugStartup
