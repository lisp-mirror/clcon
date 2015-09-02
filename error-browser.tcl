# Compilation error browser. To see it in action, execute in console:
# (clco::compile-file-for-tcl "/s2/clcon/err.lisp" nil) 
## 

# Example of message: 
# (:return
#  (:ok
#   (:compilation-result
#    ((:message "The function was called with one argument, but wants exactly zero." :severity :warning :location
#               (:location ... ... nil)
#               :references nil)
#     (:message "undefined variable: xxx" :severity :warning :location
#               (:location ... ... nil)
#               :references nil :source-context "--> PROGN SB-IMPL::%DEFUN MULTIPLE-VALUE-PROG1 PROGN \n==>\n  (BLOCK CLCO::BAR CLCO::XXX (CLCO::BAR 75))\n"))
#    nil 0.009999999776482582 t "/s2/clcon/err.fasl"))
#  15)

proc ::clconcmd::erbr {} {
    tkcon main ::erbr::SwankBrowseErrors1 {'defun}
}


namespace eval ::erbr {
    variable data
    variable tv
    variable TitleListWindow

    proc WidgetParent { w } {
        set lst [split $w .]
        set sublist [lrange $lst 0 end-1]
        puts "sublist = $sublist"
        return [join $sublist .]
    }
    
    catch {font create tkconfixed -family Courier -size -20}
    #	    $con configure -font tkconfixed

    # Replace this at integration stage
    proc ConfigureTextFonts {text} {
        $text configure \
            -foreground \#000000 \
            -insertbackground \#000000 \
            -font tkconfixed -borderwidth 1 -highlightthickness 0 \
            -undo 1
    }

    #        -background {} \


    proc InsertDataToShowOrBeep { w EventAsList } {
        # bind var for convenience

        set b [BodyTextOfErrorBrowser $w]

        # clear old data if it existed

        [TitleOfErrorBrowser $w] delete 0.0 end
        $b delete 0.0 end
        
        # and now insert what we have parsed
        
        [TitleOfErrorBrowser $w] insert 0.0 "WOW1"
        $b insert 0.0 "I don't know what is that"
    }



    # Insert text from index into detail window, and raise it
    proc RefershDetails {idx} {
        variable data
        variable tv

        set item [lindex $data $idx]

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
        $text RoDelete 0.0 end
        #    $tv.body.text RoInsert end $MyCode
        set lambda [list {w} $MyCode]
        apply $lambda [list $text]
        
        event generate $tv <<GoToTop>>
    }


    proc DoOnSelect {tbl idx} {
        after idle [RefershDetails $idx]
    }


    proc InitData { EventAsList } {
        variable data
        
        set data {}
        

    }

    proc AppendData {title DetailsCode} {
        variable TitleListWindow
        variable tv
        variable data
        set NewItem [dict create title $title DetailsCode $DetailsCode]
        lappend data $NewItem

        set tbl $TitleListWindow.tf.tbl    
        $tbl insert end [list $title]

        # If we inserted first item, highlight it
        if {[llength $data] == 1} {
            after idle "$tbl activate 0; $tbl selection set 0 0"
        }
        # InsertDataToShowOrBeep $w $EventAsList
    }

    # This is a contiuation assigned on reply on initialization request 
    proc SwankBrowseErrors1 { EventAsList } {
        # EventAsList is ignored
        variable tv

        InitData {}
        
        set w [PrepareGui1]

        EnsureTextView

        set tbl $w.tf.tbl
        
        wcb::callback $tbl before activate ::erbr::DoOnSelect
        
        #    bind $w.tf.tbl <<TablelistSelect>> [list DoOnSelect $w.tf.tbl]
        #    bind $w.tf.tbl <<TablelistCellUpdated>> [list DoOnSelect $w.tf.tbl]
        #    bind $w.tf.tbl <<ListBoxSelect>> [list DoOnSelect $w.tf.tbl]


        DoGoToTop $w
        
        focus $tbl

        # after 100 {::erbr::AppendData "error 1" {
        #     $w RoInsert 0.0 "wow";
        #     $w RoInsert end "ura"
        # }}

        # after 200 {::erbr::AppendData "error 2" {
        #     $w RoInsert 0.0 "2222"
        # }}

        return
        
    }

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
        puts "Now will call toplevel $w"
        toplevel $w
        wm title $w "Error details"
        frame $w.body
        set text $w.body.text
        text $text

        InitTextReadonly $text 1

        ConfigureTextFonts $text
        $text configure -yscrollcommand [list $w.body.sy set] 
        catch {
            # 8.5+ stuff
            set tabsp [expr {$OPT(tabspace) * [font measure $OPT(font) 0]}]
            $text configure -tabs [list $tabsp left] -tabstyle wordprocessor
        }

        #scrollbar $w.body.sx -orient h -command [list $w.body.text xview]
        scrollbar $w.body.sy -orient v -command [list $text yview]
        
        grid $text - $w.body.sy -sticky news
        #grid $w.body.sx - -sticky ew
        grid columnconfigure $w.body 0 -weight 1 
        grid columnconfigure $w.body 1 -weight 1
        grid rowconfigure $w.body 0 -weight 1 

        pack $w.body -fill both -expand 1

        bind $w <<GoToTop>> [list ::erbr::DoGoToTop $w]

        return

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
    #     $m add command -label "Save As..."  -underline 0 \
        # 	-command [list ::tkcon::Save {} widget $text]
    #     $m add command -label "Append To..."  -underline 0 \
        # 	-command [list ::tkcon::Save {} widget $text a+]
    #     $m add separator
         $m add command -label "1.Dismiss" -underline 0 -accel "Escape" -command [list destroy $w]
        bind $w <Escape>		[list destroy $w]
        #     bind $w <Control-Key-Cyrillic_tse>		[list destroy $w]
    }

    proc TitleListEditMenu {w menu} {
        set tbl [GetTitleListMenuTbl $w]
        set m [menu [::tkcon::MenuButton $menu "2.Edit" edit]]
        $m add command -label "1.Copy" -under 0 -command [list tk_textCopy $tbl] -state disabled
    #     $m add separator

        $m add command -label "2.Find" -under 0 -command [list ::erbr::TableListFindbox $tbl] -accel "Control-F" -state disabled
        bind $w <Control-Key-f> [list ::erbr::TableListFindbox $tbl]
        bind $w <Control-Key-Cyrillic_a> [list ::erbr::TableListFindbox $tbl]
    }    

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

        # ----------------------------------- menu -------------------
        
        set menu [menu $w.mbar]
        $w configure -menu $menu
        
        TitleListFileMenu $w $menu
        TitleListEditMenu $w $menu
        #TitleListInspectMenu $w $menu $w.body.text

        
        # --------------------------------- frames-----------------              

        frame $w.tf
        set tbl $w.tf.tbl
        
        tablelist::tablelist $tbl -columns {20 ""} -stretch all -spacing 10
        $tbl resetsortinfo 

        $tbl configure \
            -foreground \#000000 \
            -font tkconfixed -borderwidth 1 -highlightthickness 0
        

        $tbl columnconfigure 0 -wrap true  
        
        set f1 $w.tf
        scrollbar $f1.sy -orient v -command [list $tbl yview]
        $tbl configure -yscrollcommand [list $f1.sy set]
        grid $tbl - $f1.sy -sticky news
        grid columnconfigure $f1 0 -weight 1
        grid columnconfigure $f1 1 -weight 1
        grid rowconfigure $f1 0 -weight 1

        pack $f1 -fill both -expand 1

        return $w    
    }

    # Returns tablelist by main error browser window
    proc GetTitleListMenuTbl {w} {
        return $w.tf.tbl
    }

    proc TitleOfErrorBrowser {w} {
        set f1 $w.pane.title
        set f2 $w.pane.body
        return $w.pane.title.text
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
