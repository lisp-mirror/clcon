## Debugger

proc ::clconcmd::l {} {
    TkconSourceHere swank-io.tcl
    TkconSourceHere ldbg.tcl
}
                   
namespace eval ::ldbg {

    # Initial event which caused entering into a debugger as lisdt
    variable DebugEvent

    # list of leashed lists of two elements - short and long name of a restart
    variable Restarts 
    
    variable DbgMainWindow
    variable StackFrameHeaders

    catch {font create tkconfixed -family Courier -size -20}

    proc InitData {} {
        variable StackFrameHeaders
        set StackFrameHeaders {}
    }

    proc AppendData {name type path w} {
        variable DbgMainWindow
        variable StackFrameHeaders

        if {!([info exists DbgMainWindow]&&[winfo exists $DbgMainWindow])} {
            # There maybe no any Buffer List browser. Lets get out!
            return
        }

        set NewItem [dict create name $name type $type path $path w $w]
        lappend StackFrameHeaders $NewItem

        set tbl $DbgMainWindow.tf.tbl    
        $tbl insert end [list $name]

    }

    proc FillData { frames } {
        # FIXME frames must be a variable?
        set FramesAsList [::mprs::Unleash $frames]
        foreach lframe $FramesAsList {
            if {![::mprs::Consp $lframe]} {
                error "I thought that frame $lframe must be a cons"
            }
            set frame [::mprs::Unleash $lframe]
            # (frameid text &optional (:restartable ?))
            set w [lindex $frame 0]
            set name [::mprs::Unleash [lindex $frame 1]]
            set path $frame
            set type "."
            AppendData $name $type $path $w
        }
    }

    proc RefreshData {} {
        variable DbgMainWindow
        if {[info exists DbgMainWindow]&&[winfo exists $DbgMainWindow]} {
            ClearStackFramesTableList
        }
        InitData
        FillData
    }
        

    proc CellCmd {row action} {
        variable ::edt::EditorMRUWinList
        variable DbgMainWindow
        set p [lindex $EditorMRUWinList $row]
        set w [dict get $p w]
        switch -exact $action {
            ShowBuffer {
                ::edt::ShowExistingBuffer $w
            }
            HideListAndShowBuffer {
                wm withdraw $DbgMainWindow
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

    proc ExtractStackFrames { EventAsList } {
        set frames [lindex $EventAsList 5]
    }
    
    # This is a contiuation assigned on reply on initialization request 
    proc ldbg { EventAsList } {
        variable DebugEvent
        variable Restarts

        set DebugEvent $EventAsList
        set Restarts [ParseRestarts]
        
        InitData

        set w [PrepareGui1]

        set tbl $w.tf.tbl 

        set frames [ExtractStackFrames $EventAsList]
        FillData $frames
        #HighlightCurrentlyVisibleBuffer
        
        DoGoToTop $w
        
        focus $tbl
        
        return
        
    }

    proc DoGoToTop {w} {
        wm deiconify $w
        raise $w
    }

    # proc TitleListFileMenu {w menu} {
    #      set m [menu [::tkcon::MenuButton $menu "1.File" file]]
    #      $m add command -label "1.Close" -underline 0 -accel "Escape" -command [list destroy $w]
    #     bind $w <Escape>		[list destroy $w]
    # }


    proc CellCmdForActiveCell {tbl Cmd} {
        CellCmd [$tbl index active] $Cmd
    }

    proc DbgMainWindowBufferMenu {w menu} {
        set m [menu [::tkcon::MenuButton $menu "2.Buffer" buffer]]
        
        set ActivateCmd "::buli::CellCmdForActiveCell $w.tf.tbl HideListAndShowBuffer"
        $m add command -label "Activate" -accel "Return" -command $ActivateCmd

        set CloseCmd "::buli::CellCmdForActiveCell $w.tf.tbl CloseBuffer"
        $m add command -label "Close buffer or file" -accel "Delete" -command $CloseCmd
    }

    proc DbgMainWindowWindowMenu {w menu} {
        ## Window Menu
        ##
        set m [menu [::tkcon::MenuButton $menu "7.Window" window]]
        set cmd [list ::clconcmd::bufferlist]
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

    # Fills Restarts variable
    proc ParseRestarts {} {
        variable DebugEvent
        variable Restarts 
        set RestartsL [lindex $DebugEvent 4]
        if {![::mprs::Consp $RestartsL]} {
            error "Expected cons: $RestartsL"
        }
        set Restarts [::mprs::Unleash $RestartsL]
    }


    proc GetDebuggerThreadId {} {
        variable DebugEvent
        return [::mprs::Unleash [lindex $DebugEvent 1]]
    }

    proc GetDebuggerLevel {} {
        variable DebugEvent
        return [::mprs::Unleash [lindex $DebugEvent 2]]
    }
    
    
    # (:emacs-rex
    #  (swank:invoke-nth-restart-for-emacs 1 2)
    #  "COMMON-LISP-USER" 17 33)

    proc InvokeRestart {i} {
        variable DebugEvent
        variable Restarts
        variable DbgMainWindow
        set thread [GetDebuggerThreadId]
        set level [GetDebuggerLevel]
        ::tkcon::EvalInSwankAsync \
            "(swank:invoke-nth-restart-for-emacs $level $i)" \
            {} 0 $thread
        after idle destroy $DbgMainWindow
    }
    
    
    ## ::tkcon::HistoryMenu - dynamically build the menu for attached interpreters
    ##
    # ARGS:	m	- menu widget
    ##
    proc FillRestartsMenu m {
        variable DebugEvent
        variable Restarts
        if {![winfo exists $m]} return
        $m delete 0 end
        #
        set i 0
        foreach RestartL $Restarts {
            if {![::mprs::Consp $RestartL]} {
                error "Expected cons: $RestartL"
            }
            foreach {Short Long} [::mprs::Unleash $RestartL] {break}
            if {$i < 10} {
                set underlined 0
            } else {
                set underlined {}
            }
            $m add command \
                -label "$i: \[$Short\] $Long" \
                -command "::ldbg::InvokeRestart $i" \
                -underline $underlined
            incr i
        }
    }
    
    proc DbgMainWindowRestartsMenu {w menu} {
        variable ::tkcon::COLOR
        set m [::tkcon::MenuButton $menu "4.Restarts" restarts]
        menu $m -disabledforeground $::tkcon::COLOR(disabled) \
            -postcommand [list ::ldbg::FillRestartsMenu $m]
    }

    proc ClearStackFramesTableList {} {
        variable DbgMainWindow
        if {[winfo exists $DbgMainWindow]} {
            set tbl [::buli::GetDbgMainWindowMenuTbl $DbgMainWindow]
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
        variable DbgMainWindow

        # ---------------------------- make toplevel window DbgMainWindow -----------    
        variable ::tkcon::PRIV
        # Create unique edit window toplevel
        set w $PRIV(base).buliTlv
        if {[winfo exists $w]} {
            ClearStackFramesTableList
            return $w
        }

        set metrics [font measure tkconfixed "w"]
        toplevel $w -width [expr { 50 * $metrics }]
        wm withdraw $w
        
        # title 
        set word "Buffer list $w"
        wm title $w $word

        set DbgMainWindow $w

        # ----------------------------------- menu -------------------
        
        set menu [menu $w.mbar]
        $w configure -menu $menu
        
        # TitleListFileMenu $w $menu
        DbgMainWindowBufferMenu $w $menu
        DbgMainWindowWindowMenu $w $menu
        DbgMainWindowRestartsMenu $w $menu

        # --------------- frames, tablelist -----------------              

        frame $w.tf
        set tbl $w.tf.tbl
        
        tablelist::tablelist $tbl \
            -columns {60 "Name" left} \
            -stretch 0 -spacing 10 \
            -width 62
        $tbl resetsortinfo 

        $tbl configure \
            -foreground \#000000 \
            -font tkconfixed -borderwidth 1 -highlightthickness 1 
        

        $tbl columnconfigure 0 -wrap true  

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

        $tbl selection anchor 0

        return $w    
    }

    # Returns tablelist by main error browser window
    proc GetDbgMainWindowMenuTbl {w} {
        return $w.tf.tbl
    }

    proc TitleOfErrorBrowser {w} {
        set f1 $w.pane.title
        return $w.pane.title.text
    }
}

# ::buli::DebugStartup
