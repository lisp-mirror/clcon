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

    proc InitData { EventAsList } {
        variable data
        set data {}
    }

    # title -> key , DetailsCode -> w
    proc AppendData {title w} {
        variable TitleListWindow
        variable tv
        variable data
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
        global EditorMRUWinList
        foreach p $EditorMRUWinList {
            foreach {key w} $p break
            AppendData $key $w
        }
    }


    proc printClickedCell {w x y action} {
        foreach {tbl x y} [tablelist::convEventFields $w $x $y] break
        set row [$tbl containing $y]
        puts "clicked on cell $row"
    }

    proc printKbdCell {w x y action} {
        foreach {tbl x y} [tablelist::convEventFields $w $x $y] break
        puts $tbl
        puts [$tbl index active]
    }
    
    proc ShowSelectedBuffer {tbl HideBufferList} {
        tk_messageBox -message "ShowSelectedBuffer $tbl [$tbl index active]"        
    }
    

    # This is a contiuation assigned on reply on initialization request 
    proc BufferListBox { } {
        # EventAsList is ignored
        variable tv

        InitData {}
        
        set w [PrepareGui1]

        set tbl $w.tf.tbl 

        set bodytag [$w.tf.tbl bodytag]
        
        # wcb::callback $tbl before activate ::buli::DoOnSelect
        bind $bodytag <space> {::buli::printKbdCell %W %x %y ShowBuffer}
        bind $bodytag <Return> {::buli::printKbdCell %W %x %y HideListAndShowBuffer}
        bind $bodytag <Delete> {::buli::printKbdCell %W %x %y CloseBuffer}
        bind $bodytag <Double-Button-1> {::buli::printClickedCell %W %x %y HideListAndShowBuffer}
        
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


    proc TitleListBufferMenu {w menu} {
        set m [menu [::tkcon::MenuButton $menu "2.Buffer" buffer]]
        $m add command -label "Activate" -accel "Return" -command [list ::buli::ShowSelectedBuffer $w.tf.tbl 0]

        set CloseCmd "tk_messageBox -message 'Close'"
        $m add command -label "close" -underline 0 -command $CloseCmd
    }


    proc ClearTitleList {} {
        variable TitleListWindow
        [::buli::GetTitleListMenuTbl $TitleListWindow] delete 0 end
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
        $tbl resetsortinfo 

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
