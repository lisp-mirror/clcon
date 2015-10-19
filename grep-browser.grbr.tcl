# Grep browser. Currently it is indeed for one file only
# Backend is in grep.lisp
# Example: (clco::filter-one-file (merge-pathnames "test/dbgtest.lisp" clcon-server:*clcon-source-directory*) "defun")

# Example of message: 
#(#S(CLCO::FILTER-MATCH
#    :FILENAME #P"c:/clcon/quicklisp/local-projects/clcon/test/dbgtest.lisp"
#    :LINE-NUMBER 7
#    :LINE "(defun f (x)"
#    :START-POSITION NIL)
# I think we will parse it from the lisp.

namespace eval ::grbr {

    # dict serial -> item
    variable data

    # text view (error details)
    variable tv

    # Title list 
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
        set lambda [list {w} $ctjl]
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
    
    # Insert text from index into detail window, and raise it
    proc RefreshDetails {rowName} {
        variable data
        variable tv
        variable AutoShowSource
        variable TitleListWindow

        set item [dict get $data $rowName]

        # are we out of range?
        if { $item eq "" } {
            if { [winfo exists $tv] } {
                wm withdraw $tv
            }
            return
        }
        

        set MyCode [dict get $item {DetailsCode}]

        set text $tv.body.text
        EnsureTextView
        $text RoDelete 1.0 end
        #    $tv.body.text RoInsert end $MyCode
        set lambda [list {w} $MyCode]
        apply $lambda [list $text]
        
        event generate $tv <<GoToTop>>

	set tbl [GetTitleListMenuTbl $TitleListWindow]
        if {$AutoShowSource} {
            JumpToLocation $tbl $item 
        } else {
            after idle [subst -nocommands {
                ::erbr::DoGoToTop $TitleListWindow
                focus [$tbl bodypath]
            } 
                       ]
        }
    }


    proc DoOnSelect {tbl idx} {
        set rowName [$tbl rowcget $idx -name]
        after idle ::erbr::RefreshDetails $rowName
    }


    proc InitData { EventAsList } {
        variable data
        
        set data [dict create]
        

    }

    proc AppendData {serial severity title DetailsCode CodeToJumpToLocation} {
        variable TitleListWindow
        variable tv
        variable data
        set RowName [string cat "n" $serial]
        set NewItem [dict create                                    \
                         serial $serial                             \
                         severity $severity                         \
                         title $title                               \
                         DetailsCode $DetailsCode                   \
                         CodeToJumpToLocation $CodeToJumpToLocation \
                    ]
        dict set data $RowName $NewItem

        set tbl $TitleListWindow.tf.tbl    
        set newIndex [$tbl insert end [list $serial $severity $title]]
        $tbl rowconfigure $newIndex  -name $RowName -background [CellBackgroundColorBySeverity $severity]

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
            after idle [list ::erbr::JumpToLocation [$tbl bodypath] $dataItem]
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
                after idle [list ::erbr::GotoIndexAndMaybeShowSource $tbl $wantedAnc $ShowSource]
            }
        }
    }


    # Parses all but messages. We do not need messages at all - clco::calc-details-code does it for us. 
    proc ParseCompilationResult { EventAsList } {

        #puts stderr $EventAsList
        
        #l:compilation-result {l{l:message sThe\ function\ was\ called\ with\ one\ argument,\ but\ wants\ exactly\ zero. :severity :warning :location {l:location {l:file s/s2/clcon/test/error-browser-sample-file.lisp } {l:position n113 } yCOMMON-LISP:NIL } :references yCOMMON-LISP:NIL } {l:message sundefined\ variable:\ xxx :severity :warning :location {l:location {l:file s/s2/clcon/test/error-browser-sample-file.lisp } {l:position n91 } yCOMMON-LISP:NIL } :references yCOMMON-LISP:NIL :source-context s-->\ PROGN\ SB-IMPL::%DEFUN\ MULTIPLE-VALUE-PROG1\ PROGN\ \n==>\n\ \ (BLOCK\ CLCO::BAR\ CLCO::XXX\ (CLCO::BAR\ 75))\n } } yCOMMON-LISP:NIL n0.004 yCOMMON-LISP:NIL s/s2/clcon/test/error-browser-sample-file.fasl 

        # (defstruct (:compilation-result
        #              (:type list) :named)
        # 1  notes
        # 2  (successp nil :type boolean)
        # 3  (duration 0.0 :type float)
        # 4  (loadp nil :type boolean)
        # 5  (faslfile nil :type (or null string)))

        variable successp
        variable duration
        variable faslfile
        variable ForceLoadingMakesSence 

        set e $EventAsList
        set successp [expr {![::mprs::Null [lindex $e 2]]}]
        set duration [::mprs::Unleash [lindex $e 3]]
        set lFaslfile [lindex $e 5]

        if {[::mprs::Null $lFaslfile]} {
            set faslfile {}
        } else {
            set faslfile [::mprs::Unleash $lFaslfile]
        }

        set ForceLoadingMakesSence [expr {!$successp && ($faslfile ne {})}]
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

    proc ForceLoad {} {
        variable ForceLoadingMakesSence
        variable faslfile
        if {!$ForceLoadingMakesSence} {
            return
        }
        set qFaslfile [::tkcon::QuoteLispObjToString $faslfile]
        ::tkcon::FocusConsole
        puts "Force loading $faslfile..."
        # ::tkcon::EvalInSwankAsync 
        ::tkcon::SendEventToSwank "(common-lisp:load $qFaslfile)" \
            {::erbr::CloseErrorBrowser} 1 {:find-existing}
    }
    
    proc FillHeaderText { clcon_text } {
        variable successp
        variable duration
        variable faslfile
        variable ForceLoadingMakesSence
        set t $clcon_text
        $t RoDelete 1.0 end
        $t tag configure RedText -foreground red
        if {$ForceLoadingMakesSence} {
            $t RoInsert end "Compilation failed. Press ! to load fasl anyway\n" {RedText}
            $t RoInsert end [string cat $faslfile \n]
        } elseif {!$successp} {
            $t RoInsert end "Compilation failed\n" {RedText}
        }
        $t RoInsert end "Compilation took $duration s\n"
    }

    # This is a contiuation assigned on reply on initialization request 
    proc SwankBrowseErrors1 { EventAsList } {
        # EventAsList is ignored
        variable tv

        InitData {}
        
        set w [PrepareGui1]

        ParseCompilationResult $EventAsList
        
        FillHeaderText $w.header.text 

        EnsureTextView

        set tbl $w.tf.tbl
        
        wcb::callback $tbl before activate ::erbr::DoOnSelect
        
        #    bind $w.tf.tbl <<TablelistSelect>> [list DoOnSelect $w.tf.tbl]
        #    bind $w.tf.tbl <<TablelistCellUpdated>> [list DoOnSelect $w.tf.tbl]
        #    bind $w.tf.tbl <<ListBoxSelect>> [list DoOnSelect $w.tf.tbl]


        DoGoToTop $w
        
        focus [$tbl bodypath]

        # after 100 {::erbr::AppendData "error 1" {
        #     $w RoInsert 1.0 "wow";
        #     $w RoInsert end "ura"
        # }}

        # after 200 {::erbr::AppendData "error 2" {
        #     $w RoInsert 1.0 "2222"
        # }}

        return
        
    }

    # Raises toplevel window
    proc DoGoToTop {w} {
        wm deiconify $w
        raise $w
        #tk_messageBox -message "WOW"
    }

    proc EnsureTextView {} {
        variable ::tkcon::PRIV
        # Create unique edit window toplevel
        variable tv
        set tv $PRIV(base).erbrTv
        set w $tv

        if {[winfo exists $tv]} {
            return
        }
        putd "Now will call toplevel $w"
        toplevel $w
        wm title $w "Error details"
        bind $w <Escape> [list destroy $w]

        ::gui_util::frame_clcon_text_and_vertical_scrollbar $w.body {-readonly 1}
        
        pack $w.body -fill both -expand 1

        bind $w <<GoToTop>> [list ::erbr::DoGoToTop $w]

        return

    }

    # It is reasonable to sort by severity first, then by number
    # How do we transform severity to number? -sortmode , -sortcommand for column
    proc DefaultSortHeaders {tbl} {
        $tbl sortbycolumnlist {1 0} {decreasing increasing}
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
    proc AddNextAndPreviousCompilerMessagesCommands {menu tagListForKeys ShowSource} {
        set m $menu

        set cmdBack [list ::erbr::EditOtherCompilerMessage -1 $ShowSource]
        $m add command -label "Goto prev compiler message" -command $cmdBack -accel "Alt-F7"

        set cmdForward [list ::erbr::EditOtherCompilerMessage 1 $ShowSource]
        $m add command -label "Goto next compiler message" -command $cmdForward -accel "Alt-F8"
        foreach tag $tagListForKeys {
            # puts stderr $tag
            bind $tag <Alt-Key-F7> $cmdBack
            bind $tag <Alt-Key-F8> $cmdForward
        }
    }
    
    proc TitleListFileMenu {w menu} {
        set m [menu [::tkcon::MenuButton $menu "1.File" file]]
        #     $m add command -label "Save As..."  -underline 0 \
            # 	-command [list ::tkcon::Save {} widget $text]
    #     $m add command -label "Append To..."  -underline 0 \
            # 	-command [list ::tkcon::Save {} widget $text a+]
        #     $m add separator
        $m add command -label "1.Dismiss" -underline 0 -accel "Escape" -command [list destroy $w]
        bind $w <Escape>		[list destroy $w]
        #     bind $w <Control-Key-Cyrillic_tse>		[list destroy $w]

        set cmd ::erbr::ForceLoad
        $m add command -label "!Force load fasl from failed compilation" -underline 0 -command $cmd
        bind $w.header.text <exclam> $cmd
        set bodytag [$w.tf.tbl bodytag]
        bind $bodytag <exclam> $cmd
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

        set cmd [list ::erbr::JumpToCurrentLocation $tbl]
        $m add command -label "Jump to current source location" -accel "<space>" -command $cmd
        foreach tag [list [$tbl bodytag] $text] {
            bind $tag <space> $cmd
        }
        
        AddNextAndPreviousCompilerMessagesCommands $m [list [$tbl bodytag] $text] 0

        
    }

    # proc SortOrderBooleanToWord {x} {
    #     if {$x == 0} { decreasing } else { increasing }
    # }

    # Unused 
    # proc TitleListSortMenu {w menu} {
    #     variable InDecreasingOrder
    #     set tbl [GetTitleListMenuTbl $w]
    #     set m [menu [::tkcon::MenuButton $menu "3.Sort" sort]]
    #     $m add check -label "0.In decreasing order" -underline 0 -variable ::erbr::InDecreasingOrder -command {if {$::erbr::InDecreasingOrder} }
    #     $m add check -label "1.№" -underline 0
    #     $m add check -label "2.Severity" -underline 0

    #     puts stderr [$tbl labeltag 0]
    #     bind [$tbl labeltag 0] <ButtonRelease-1> "puts Fuck"
    # }


                

    # proc TitleListInspectMenu {w menu text} {
    #     set m [menu [::tkcon::MenuButton $menu "3.Inspect" inspect]]

    #     $m add command -label "Back" -accelerator <BackSpace> -command [list ::insp::InspectorPop $w]
    #     bind $w <BackSpace> [list ::insp::InspectorPop $w]
    #     bind $w <Alt-Key-Left> [list ::insp::InspectorPop $w]

    #     $m add command -label "Forward" -accelerator <Alt-Key-Right> -command [list ::insp::InspectorNext $w]
    #     bind $w <Alt-Key-Right> [list ::insp::InspectorNext $w]

    #     $m add command -label "Refresh" -accelerator <F5> -command [list ::insp::InspectorReinspect $w]
    #     bind $w <F5> [list ::insp::InspectorReinspect $w]
    
    # }    

    proc ClearTitleList {} {
        variable TitleListWindow
        
        [::erbr::GetTitleListMenuTbl $TitleListWindow] delete 0 end
    }

        
    proc CompareSeveritiesBySeverityNumber {x y} {
        set xN [SeverityToSeverityNumber $x]
        set yN [SeverityToSeverityNumber $y]
        if {$xN<$yN} {
            return -1
        } elseif {$xN==$yN} {
            return 0
        } else {return 1}
    }

  
    # Make toplevel widget and its children 
    proc PrepareGui1 {} {
        variable TitleListWindow

        # ---------------------------- make toplevel window TitleListWindow -----------    
        variable ::tkcon::PRIV
        # Create unique edit window toplevel
        set w $PRIV(base).erbrTlv
        puts $w
        if {[winfo exists $w]} {
            ClearTitleList
            return $w
        }

        toplevel $w
        wm withdraw $w
        
        # title 
        set word "Error browser"
        if {[string length $word] > 20} {
            wm title $w "[string range $word 0 16]... - tkcon Edit"
        } else {
            wm title $w "$word - tkcon Edit"
        }

        set TitleListWindow $w

        # --------------------------------- frames-----------------              

        ::gui_util::frame_clcon_text_and_scrollbars $w.header {-readonly 1 -height 6}
        
        frame $w.tf
        set tbl $w.tf.tbl
        
        tablelist::tablelist $tbl -columns {3 "№" 12 "Severity" 20 "Text"} -stretch 2 -spacing 10
        # $tbl resetsortinfo

        $tbl configure \
            -foreground \#000000 \
            -font tkconfixed -borderwidth 1 -highlightthickness 0
        

        $tbl columnconfigure 0 -sortmode integer
        $tbl columnconfigure 1 \
            -sortmode command \
            -sortcommand ::erbr::CompareSeveritiesBySeverityNumber
        $tbl columnconfigure 2 -wrap true

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
        #TitleListInspectMenu $w $menu $w.body.text

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

    proc DebugStartup {} {
        SwankBrowseErrors1 {'defun}
    }

}

# ::erbr::DebugStartup
