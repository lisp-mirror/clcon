## Buffer-list
## Design for the future: we will have several windows, but every buffer is
## bound to only one window. Sometimes we might try to use peering text widgets
## to clone some buffers to several windows, which is useful sometimes.

namespace eval ::buli {

    # contents of widget separated from widget
    variable data
    variable TitleListWindow

    catch {font create tkconfixed -family Courier -size -20}

    proc InitData {} {
        variable data
        set data {}
    }



    # There is a design problem. We mix visual and non-visual
    # activity while we don't know if visual component exists
    # We recreate non-visual data at creation of visual control
    # That seem to be a bad design. On the other hand, we would like
    # to be able to change contents of visual component incrementally
    # This was desired for error-browser and this can also be desired
    # for buffer-list. E.g. we would like to remove deleted window
    # without moving keyboard focus on the list.

    # Bi is a buffer index
    # Mod is modified flag
    proc AppendData {name type path Bi mod} {
        variable TitleListWindow
        variable data

        if {!([info exists TitleListWindow]&&[winfo exists $TitleListWindow])} {
            # There maybe no any Buffer List browser. Lets get out!
            return
        }

        set NewItem [dict create name $name type $type path $path Bi $Bi $mod mod]
        lappend data $NewItem

        set tbl $TitleListWindow.tf.tbl    
        $tbl insert end [list $mod $name $type $path]

    }

    proc FillData {} {
        variable ::edt::EditorMRUWinList
        foreach p $EditorMRUWinList {
            set name [dict get $p name]
            set type [dict get $p type]
            set path [dict get $p path]
            set Bi [dict get $p Bi]
            set modBoolean [[::edt::Bi2btext $Bi] edit modified]
            set mod [BooleanToAsterik $modBoolean]
            AppendData $name $type $path $Bi $mod
        }
    }

    proc HighlightCurrentlyVisibleBuffer {} {
        variable EditorMRUWinList
        variable data
        variable TitleListWindow
        set cBi [::edt::cBi]
        if {$cBi eq {}} return
        set i 0
        foreach d $data {
            set thisBi [dict get $d Bi]
            if {$thisBi eq $cBi} {
                set tbl $TitleListWindow.tf.tbl
                after idle "$tbl activate $i; $tbl selection set $i $i"
                return
            }
            incr i
        }
    }

    
    proc RefreshData {} {
        variable TitleListWindow
        if {[info exists TitleListWindow]&&[winfo exists $TitleListWindow]} {
            ClearTitleList
        }
        InitData
        FillData
        HighlightCurrentlyVisibleBuffer
    }
        

    proc CellCmd {row action} {
        variable ::edt::EditorMRUWinList
        variable TitleListWindow
        set p [lindex $EditorMRUWinList $row]
        set Bi [dict get $p Bi]
        switch -exact $action {
            ShowBuffer {
                ::edt::SwitchToBuffer $Bi focus
            }
            HideListAndShowBuffer {
                wm withdraw $TitleListWindow
                ::edt::SwitchToBuffer $Bi deiconify
            }
            CloseBuffer {
                ::edt::EditCloseFile $Bi
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
    
    # This is a contiuation assigned on reply on initialization request 
    proc BufferListBox { } {
        # EventAsList is ignored

        InitData

        set w [PrepareGui1]

        set tbl $w.tf.tbl 

        FillData
        HighlightCurrentlyVisibleBuffer
        
        DoGoToTop $w
        
        focus [$tbl bodypath]
        
        return
        
    }

    proc DoGoToTop {w} {
        wm deiconify $w
        raise $w
    }

    # # Fills browser from data 
    # proc FillHeaders {tbl} {
    #     variable data
    #     foreach item $data {
    #         set title [dict get $item {title}]
    #         $tbl insert end [list $title]
    #     }
    # }

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

    proc TitleListWindowMenu {w menu} {
        ## Window Menu
        ##
        set m [menu [::tkcon::MenuButton $menu "7.Window" window]]
        set cmd ::buli::BufferListBox
	$m add command -label "Buffer list" -accel "Control-F12" \
            -command $cmd -state disabled 
        #bind $w <Control-Key-F12> $cmd
        #
        set cmd [list ::tkcon::FocusConsole]
	$m add command -label "Console" -accel "Control-." \
            -command $cmd
        bind $w <Control-Key-period> $cmd
        bind $w <Control-Key-Cyrillic_yu> $cmd
        #
        set cmd [list ::edt::ShowSomeEditor]
        $m add command -label "Editor" -accel "Control-Shift-e" \
            -command $cmd
        bind $w <Control-Shift-E> $cmd
        bind $w <Control-Shift-Key-Cyrillic_U> $cmd
    }
    

    proc ClearTitleList {} {
        variable TitleListWindow
        if {[winfo exists $TitleListWindow]} {
            set tbl [::buli::GetTitleListMenuTbl $TitleListWindow]
            $tbl delete 0 end
        }
    }


    proc MakeBindings {w} {
        set bodytag [$w.tf.tbl bodytag]
        
        # wcb::callback $tbl before activate ::buli::DoOnSelect
        bind $bodytag <space> {::buli::KbdCellCmd %W %x %y ShowBuffer; break}
        bind $bodytag <Return> {::buli::KbdCellCmd %W %x %y HideListAndShowBuffer; break}
        bind $bodytag <Delete> {::buli::KbdCellCmd %W %x %y CloseBuffer; break}
        bind $bodytag <Double-Button-1> {::buli::MouseCellCmd %W %x %y HideListAndShowBuffer; break}
        
        #    bind $w.tf.tbl <<TablelistCellUpdated>> [list DoOnSelect $w.tf.tbl]
        #    bind $w.tf.tbl <<ListBoxSelect>> [list DoOnSelect $w.tf.tbl]
    }

    
    # Make toplevel widget and its children
    # Returns window
    proc PrepareGui1 {} {
        variable TitleListWindow

        # ---------------------------- make toplevel window TitleListWindow -----------    
        variable ::tkcon::PRIV
        # Create unique edit window toplevel
        set w $PRIV(base).buliTlv
        if {[winfo exists $w]} {
            ClearTitleList
            return $w
        }

        set metrics [font measure tkconfixed "w"]
        toplevel $w -width [expr { 50 * $metrics }]
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
        TitleListWindowMenu $w $menu

        # --------------- frames, tablelist -----------------              

        frame $w.tf
        set tbl $w.tf.tbl
        
        tablelist::tablelist $tbl \
            -columns {3 "Mod" 20 "Name" left 3 "Typ" left 30 "Path" left} \
            -width 40
        $tbl resetsortinfo 

        $tbl configure \
            -foreground \#000000 \
            -font tkconfixed -borderwidth 1 -highlightthickness 1 
        

        $tbl columnconfigure 1 -wrap true  
        $tbl columnconfigure 3 -wrap true

        # ------------------------------ bindings -------------
        MakeBindings $w
        
        # -------------------------- layout -------------------
        
        set f1 $w.tf
        scrollbar $f1.sy -orient v -command [list $tbl yview]
        $tbl configure -yscrollcommand [list $f1.sy set]
        grid $tbl - $f1.sy -sticky news
        grid columnconfigure $f1 0 -weight 1
        grid columnconfigure $f1 1 -weight 0
        grid rowconfigure $f1 0 -weight 1

        pack $f1 -fill both -expand 1

        ::tablelist_util::TreeSetTo $tbl 0

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
