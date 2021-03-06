# Grep browser. Currently it is indeed for one file only
# Backend is in grep.lisp
# Example1: (clco::present-text-filtering-results (clco::filter-one-file (merge-pathnames "test/dbgtest.lisp" clcon-server:*clcon-source-directory*) "defun"))
# Example2: (clco::find-in-clcon-sources "defun") 

package require snit

namespace eval ::grbr {

    ::snit::widgetadaptor grep_browser {
        # dict serial -> item
        option -data -default [dict create]
        constructor {args} {
            installhull using toplevel
        }
        method set_font {size} {
            variable ::tkcon::OPT
            set font [lindex $::tkcon::OPT(шрифты) $size]
            [HeaderOfGrepBrowser $self] configure -font $font
            [GetTitleListMenuTbl $self] configure -font $font
        }
        delegate method * to hull
        delegate option * to hull
    }

    
    # Returns tablelist by main error browser window
    proc GetTitleListMenuTbl {grbr} {
        return $grbr.tf.tbl
    }

        
    proc HeaderOfGrepBrowser {grbr} {
        return $grbr.header.text 
    }

    proc JumpToLocation {tbl dataItem} {
        set ctjl [dict get $dataItem {CodeToJumpToLocation}]
        # It would be nice to process errors occured when opening files
        set lambda [list {w_unused} $ctjl]
        apply $lambda [list $tbl]  
    }

    # Jump to current source location if it is possible.
    # Otherwise, issue a warning and stay in the message list
    proc JumpToCurrentLocation {grbr args} {
        named_args $args {-close 0}
        set data [$grbr cget -data]
        set tbl [GetTitleListMenuTbl $grbr]
        set rowName [$tbl rowcget active -name]
        set dataItem [dict get $data $rowName]
        JumpToLocation [$tbl bodypath] $dataItem
        if {$(-close)} {
            after idle [list destroy $grbr]
        }
    }
    

    proc DoOnSelect {tbl idx} {
        set rowName [$tbl rowcget $idx -name]
    }


    proc InitData { grbr } {
        $grbr configure -data [dict create]
    }

    proc AppendData {grbr serial line filename LineNumber StartPosition EndPosition CodeToJumpToLocation} {
        set TitleListWindow $grbr
        set data [$grbr cget -data]
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

        # See also ::edt::PrepareGui1 for columns configuration
        set newIndex [$tbl insert end [list $serial $line $LineNumber $filename]]
        $tbl rowconfigure $newIndex -name $RowName

        # If we inserted first item, highlight it
        if {[dict size $data] == 1} {
            after idle [list ::tablelist_util::GotoIndex $tbl 0]
        }

        # DefaultSortHeaders $tbl

        $grbr configure -data $data
        # InsertDataToShowOrBeep $w $EventAsList
    }

    proc GotoIndexAndMaybeShowSource {grbr tbl wantedAnc ShowSource} {
        set data [$grbr cget -data]
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

    # Unused (yet)
    proc EditOtherCompilerMessage {grbr increment ShowSource} {
        set TitleListWindow $grbr
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

    proc FillHeaderText { grbr string } {
        set text [HeaderOfGrepBrowser $grbr]
        ::ro_out::D $text 1.0 end
        ::ro_out::I $text 1.0 $string
        set TitleShort [string range $string 0 50]
        set TitleShortNoSpaces [regsub -line -all \n $TitleShort " "]
        wm title $grbr $TitleShortNoSpaces
    }

    # Called from lisp, see clco::present-text-filtering-results
    # Returns (toplevel = grep_browser) widget pathname
    proc OpenGrepBrowser { } {

        set grbr [MakeNewGrbr]

        InitData $grbr
        
        PrepareGui1 $grbr

        # FillHeaderText $grbr [HeaderOfGrepBrowser $grbr]

        set tbl [GetTitleListMenuTbl $grbr]
        
        wcb::callback $tbl before activate ::grbr::DoOnSelect

        return $grbr
    }

    proc ShowGrepBrowser {grbr} {

        set tbl [GetTitleListMenuTbl $grbr]

        DefaultSortHeaders $tbl
        ::win_lay::PositionATool $grbr
        ::erbr::DoGoToTop $grbr
       
        focus [$tbl bodypath]

        return $grbr
    }

    # It is reasonable to sort by severity first, then by number
    # How do we transform severity to number? -sortmode , -sortcommand for column
    proc DefaultSortHeaders {tbl} {
        $tbl sortbycolumnlist {0} {increasing}
        $tbl see active
    }
    
    # Fills browser from data 
    proc FillHeaders {grbr tbl} {
        set data [$grbr cget -data]
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
        $m add command -label "Предыдущая находка" -command $cmdBack -accel "Alt-F7"

        set cmdForward [list ::grbr::EditOtherCompilerMessage 1 $ShowSource]
        $m add command -label "Следующая находка" -command $cmdForward -accel "Alt-F8"
        foreach tag $tagListForKeys {
            # puts stderr $tag
            bind $tag <Alt-Key-F7> $cmdBack
            bind $tag <Alt-Key-F8> $cmdForward
        }
    }
    
    proc TitleListFileMenu {w menu} {
        set m [menu [::tkcon::MenuButton $menu "1.Файл" file]]
        $m add command -label "1.Закрыть" -underline 0 -accel "Escape" -command [list destroy $w]
        bind $w <Escape>		[list destroy $w]
        ::clcon_key::b bind $w <Control-Key-w> [list destroy $w]
    }

    proc TitleListEditMenu {grbr menu} {
        set w $grbr
        set tbl [GetTitleListMenuTbl $w]
        set text [HeaderOfGrepBrowser $w]
        set m [menu [::tkcon::MenuButton $menu "2.Правка" edit]]
        $m add command -label "1.Копировать" -under 0 -command [list tk_textCopy $tbl] -state disabled
    #     $m add separator

        set cmd [list ::fndrpl::OpenFindBox $tbl "tablelist" "find" {}]
        $m add command -label "Поиск" -under 0 -command $cmd -accel "Control-F" 
        ::clcon_key::b bind $w <Control-Key-f> $cmd
        $m add separator

        set cmd [list ::grbr::JumpToCurrentLocation $grbr -close 0]
        $m add command -label "Показать исходный текст" -accel "<Space>" -command $cmd
        foreach tag [list [$tbl bodytag] $text] {
            bind $tag <space> $cmd
        }

        set cmd [list ::grbr::JumpToCurrentLocation $grbr -close 1]
        $m add command -label "Закрыть список находок и показать исходный текст " -accel "<Return>" -command $cmd
        foreach tag [list [$tbl bodytag] $text] {
            bind $tag <Return> [concat $cmd ";" break]
        }
        
        AddNextAndPreviousMatchCommands $m [list [$tbl bodytag] $text] 0

        $m add separator

        ::tkcon::ВставитьВМенюПунктыПроШрифты $m $w {{Виджет КодРазмера} {
            ${Виджет} set_font ${КодРазмера}}
        }
    }


    proc TitleListWindowMenu {w menu} {
        variable ::tkcon::COLOR
        set tbl [GetTitleListMenuTbl $w]
        set text [HeaderOfGrepBrowser $w]
        set m [::tkcon::MenuButton $menu "7.Окно" window]
	menu $m -disabledforeground $COLOR(disabled) \
		-postcommand [list ::window_menu::DynamicWindowMenu $w $m]
        ::window_menu::WindowMenuKeyBindings $w $w $w
    }


    proc ClearTitleList {grbr} {
        set TitleListWindow $grbr
        
        [::grbr::GetTitleListMenuTbl $TitleListWindow] delete 0 end
    }


    # Creates grbr and returns it
    proc MakeNewGrbr {  } {
        variable ::tkcon::PRIV

        set GrBrId [GenNamedCounter "grep_browser"]
       
        set w [string cat $PRIV(base) ".grbrTlv_" $GrBrId]

        # puts $w
        if {[winfo exists $w]} {
            error "Окно просмотровщика находок $w уже существует"
        }

        grep_browser $w
        wm withdraw $w
        
        return $w
    }

    # Make toplevel widget and its children 
    proc PrepareGui1 {grbr} {
        
        set TitleListWindow $grbr
        set w $grbr


        # title 
        set word "Просмотровщик находок в файлах"
	wm title $w "$word"

        set TitleListWindow $w

        # --------------------------------- frames-----------------              

        ::gui_util::frame_clcon_text_and_scrollbars $w.header {-readonly 1 -height 2}
        
        frame $w.tf
        set tbl $w.tf.tbl
        
        # When you change column order, see also:
        # ::edt::AppendData , ::edt::DefaultSortHeaders
        tablelist::tablelist $tbl -columns {4 "№" 50 "Line" 4 "Line No" 25 "File"} -spacing 10 \
        -exportselection 0 -stripebackground \#F3F3F3

        # $tbl resetsortinfo

        $tbl columnconfigure 0 -sortmode integer
        $tbl columnconfigure 1 -wrap true
        $tbl columnconfigure 2 -sortmode integer
        $tbl columnconfigure 3 -align right


        ::tablelist_util::BindReSortingToClickingOnColumnLabel $tbl

        # -font tkconfixed
        $tbl configure \
            -foreground \#000000 \
            -font [::tkcon font] \
            -borderwidth 1 -highlightthickness 0
        
        
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
        TitleListWindowMenu $w $menu

        return $w    
    }

}

