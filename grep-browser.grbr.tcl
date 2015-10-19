# Grep browser. Currently it is indeed for one file only
# Backend is in grep.lisp
# Example: (clco::present-text-filtering-results (clco::filter-one-file (merge-pathnames "test/dbgtest.lisp" clcon-server:*clcon-source-directory*) "defun"))
# Example2: (clco::find-in-clcon-sources "defun") 

namespace eval ::grbr {

    # dict serial -> item
    variable data

    variable TitleListWindow 

    # Compilation successful 
    variable successp

    # Duration, sec
    variable duration

    # Fasl file 
    variable faslfile

    # Compilation failed, but fasl can be loaded
    variable ForceLoadingMakesSence 0


    # If 1, source is shown as entry is selected in TitleList
    variable AutoShowSource 0
    

    proc WidgetParent { w } {
        set lst [split $w .]
        set sublist [lrange $lst 0 end-1]
        puts "sublist = $sublist"
        return [join $sublist .]
    }
    
    catch {font create tkconfixed -family Courier -size -20}
    #	    $con configure -font tkconfixed

    #        -background {} \


    proc JumpToLocation {tbl dataItem} {
        set ctjl [dict get $dataItem {CodeToJumpToLocation}]
        # It would be nice to process errors occured when opening files
        set lambda [list {w_unused} $ctjl]
        apply $lambda [list $tbl]  
    }

    # Jump to current source location if it is possible.
    # Otherwise, issue a warning and stay in the message list
    proc JumpToCurrentLocation {tbl} {
        variable data
        set rowName [$tbl rowcget active -name]
        set dataItem [dict get $data $rowName]
        JumpToLocation [$tbl bodypath] $dataItem
    }
    

    proc DoOnSelect {tbl idx} {
        set rowName [$tbl rowcget $idx -name]
    }


    proc InitData { EventAsList } {
        variable data
        
        set data [dict create]
        

    }

    proc AppendData {serial line filename LineNumber StartPosition EndPosition CodeToJumpToLocation} {
        variable TitleListWindow
        variable tv
        variable data
        set RowName [string cat "n" $serial]
        set NewItem [dict create                                    \
                         serial $serial                             \
                         line $line                                 \
                         filename $filename                         \
                         LineNumber $LineNumber                     \
                         StartPosition $StartPosition               \
                         EndPosition $EndPosition                   \
                         CodeToJumpToLocation $CodeToJumpToLocation \
                    ]
        dict set data $RowName $NewItem

        set tbl $TitleListWindow.tf.tbl    
        set newIndex [$tbl insert end [list $line $LineNumber $filename $serial]]
        $tbl rowconfigure $newIndex -name $RowName

        # If we inserted first item, highlight it
        if {[dict size $data] == 1} {
            after idle [list ::tablelist_util::GotoIndex $tbl 0]
        }

        DefaultSortHeaders $tbl
        # InsertDataToShowOrBeep $w $EventAsList
    }

    proc GotoIndexAndMaybeShowSource {tbl wantedAnc ShowSource} {
        variable data
        ::tablelist_util::GotoIndex $tbl $wantedAnc
        # It is important that JumpToLocation is issued
        # after GotoIndex which shedules error detail window
        # to pop up.
        if {$ShowSource} {
            set rowName [$tbl rowcget $wantedAnc -name]
            set dataItem [dict get $data $rowName]
            after idle [list ::grbr::JumpToLocation [$tbl bodypath] $dataItem]
        }
    }
    
    proc EditOtherCompilerMessage {increment ShowSource} {
        variable TitleListWindow
        if {[winfo exists $TitleListWindow]} {
            set tbl $TitleListWindow.tf.tbl
            set anc [$tbl index active]
            set wantedAnc [expr {$anc + $increment}]
            if {$wantedAnc < 0} {
                bell
            } elseif {$wantedAnc >= [$tbl size]} {
                bell
            } else {
                after idle [list ::grbr::GotoIndexAndMaybeShowSource $tbl $wantedAnc $ShowSource]
            }
        }
    }

    # It is unspecified (yet) whether we destroy it. Let's delete them for debugging purposes
    proc CloseErrorBrowser {} {
        variable TitleListWindow
        variable tv
        if {[winfo exists $tv]} {
            wm withdraw $tv
            after idle [list destroy $tv]
        }
        if {[winfo exists $TitleListWindow]} {
            wm withdraw $TitleListWindow
            after idle [list destroy $TitleListWindow]
        }
    }

    proc FillHeaderText { clcon_text } {
    }

    # Called from lisp, see clco::present-text-filtering-results
    proc OpenGrepBrowser { } {
        variable tv

        InitData {}
        
        set w [PrepareGui1]

        FillHeaderText $w.header.text 

        set tbl $w.tf.tbl
        
        wcb::callback $tbl before activate ::grbr::DoOnSelect
        
        ::erbr::DoGoToTop $w
        
        focus [$tbl bodypath]

        return
        
    }

    # It is reasonable to sort by severity first, then by number
    # How do we transform severity to number? -sortmode , -sortcommand for column
    proc DefaultSortHeaders {tbl} {
        $tbl sortbycolumnlist {3} {increasing}
        $tbl see active
    }
    
    # Fills browser from data 
    proc FillHeaders {tbl} {
        variable data
        dict map {rowName item} $data {
            set title [dict get $item {title}]
            $tbl insert end [list $title]
        }
        DefaultSortHeaders $tbl
    }

    # If ShowSource == 1, shows source after jumping
    # We would like also to add this to editor, but later... FIXME
    proc AddNextAndPreviousMatchCommands {menu tagListForKeys ShowSource} {
        set m $menu

        set cmdBack [list ::grbr::EditOtherCompilerMessage -1 $ShowSource]
        $m add command -label "Goto prev match" -command $cmdBack -accel "Alt-F7"

        set cmdForward [list ::grbr::EditOtherCompilerMessage 1 $ShowSource]
        $m add command -label "Goto next match" -command $cmdForward -accel "Alt-F8"
        foreach tag $tagListForKeys {
            # puts stderr $tag
            bind $tag <Alt-Key-F7> $cmdBack
            bind $tag <Alt-Key-F8> $cmdForward
        }
    }
    
    proc TitleListFileMenu {w menu} {
        set m [menu [::tkcon::MenuButton $menu "1.File" file]]
        $m add command -label "1.Dismiss" -underline 0 -accel "Escape" -command [list destroy $w]
        bind $w <Escape>		[list destroy $w]
        bind $w <Control-Key-w>		[list destroy $w]
    }

    proc TitleListEditMenu {w menu} {
        set tbl [GetTitleListMenuTbl $w]
        set text [HeaderOfErrorBrowser $w]
        set m [menu [::tkcon::MenuButton $menu "2.Edit" edit]]
        $m add command -label "1.Copy" -under 0 -command [list tk_textCopy $tbl] -state disabled
    #     $m add separator

        set cmd [list ::fndrpl::OpenFindBox $tbl "tablelist" "find" {}]
        $m add command -label "2.Find" -under 0 -command $cmd -accel "Control-F" 
        bind $w <Control-Key-f> $cmd
        bind $w <Control-Key-Cyrillic_a> $cmd

        $m add separator

        set cmd [list ::grbr::JumpToCurrentLocation $tbl]
        $m add command -label "Jump to current source location" -accel "<space>" -command $cmd
        foreach tag [list [$tbl bodytag] $text] {
            bind $tag <space> $cmd
        }
        
        AddNextAndPreviousMatchCommands $m [list [$tbl bodytag] $text] 0

        
    }

    proc ClearTitleList {} {
        variable TitleListWindow
        
        [::grbr::GetTitleListMenuTbl $TitleListWindow] delete 0 end
    }

        
    # Make toplevel widget and its children 
    proc PrepareGui1 {} {
        variable TitleListWindow

        # ---------------------------- make toplevel window TitleListWindow -----------    
        variable ::tkcon::PRIV
        # Create unique edit window toplevel
        
        # set GrBrId [GenNamedCounter "grbrTlv"]
        set GrBrId ""
	
        set w [string cat $PRIV(base) ".grbrTlv" $GrBrId]


        # puts $w
        if {[winfo exists $w]} {
            ClearTitleList
            return $w
        }

        toplevel $w
        wm withdraw $w
        
        # title 
        set word "File search matches browser"
	wm title $w "$word"

        set TitleListWindow $w

        # --------------------------------- frames-----------------              

        ::gui_util::frame_clcon_text_and_scrollbars $w.header {-readonly 1 -height 6}
        
        frame $w.tf
        set tbl $w.tf.tbl
        
        tablelist::tablelist $tbl -columns {30 "Line" 4 "Line No" 45 "File" 4 "Serial"} -stretch 0 -spacing 10
        # $tbl resetsortinfo

        $tbl configure \
            -foreground \#000000 \
            -font tkconfixed -borderwidth 1 -highlightthickness 0
        

        $tbl columnconfigure 0 -wrap true
        $tbl columnconfigure 1 -sortmode integer
        $tbl columnconfigure 2 -wrap true
        $tbl columnconfigure 3 -sortmode integer

        ::tablelist_util::BindReSortingToClickingOnColumnLabel $tbl

        
        set f1 $w.tf
        scrollbar $f1.sy -orient v -command [list $tbl yview]
        $tbl configure -yscrollcommand [list $f1.sy set]
        grid $tbl - $f1.sy -sticky news
        grid columnconfigure $f1 0 -weight 1
        grid columnconfigure $f1 1 -weight 1
        grid rowconfigure $f1 0 -weight 1

        pack $w.header -side top -fill x
        pack $f1 -fill both -expand 1

        # ----------------------------------- menu and bindings ------------
        # It must be below frames as we bind keys to them
        
        set menu [menu $w.mbar]
        $w configure -menu $menu
        
        TitleListFileMenu $w $menu
        TitleListEditMenu $w $menu

        return $w    
    }

    # Returns tablelist by main error browser window
    proc GetTitleListMenuTbl {w} {
        return $w.tf.tbl
    }

        
    proc HeaderOfErrorBrowser {w} {
        return $w.header.text 
    }

    proc BodyTextOfErrorBrowser {w} {
        set f1 $w.pane.title
        set f2 $w.pane.body
        return $f2.text
    }

}

