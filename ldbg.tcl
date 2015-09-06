## Debugger
# TkconSourceHere search-tablelist.tcl

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

    # Creates a tablelist -name row option value for a frame number
    proc FrameNumberToRowName {n} {
        string cat "fr" $n
    }

    # Args:
    # RowName - -name option from tablelist row
    # Returns:
    # index of item in StackFrameHeaders with that RowName
    proc GetStackFrameHeaderIndexByRowName {RowName} {
        variable StackFrameHeaders
        set i 0 
        foreach h $StackFrameHeaders {
            if {[dict get $h RowName] eq $RowName} {
                return $i
            }
            incr i
        }
        error "GetStackFrameHeaderIndexByRowName failed at $RowName"
    }

    proc RowNameToFrameNo {RowName} {
        variable StackFrameHeaders
        set i [GetStackFrameHeaderIndexByRowName $RowName]
        set item [lindex $StackFrameHeaders $i]
        dict get $item FrameNo
    }        
    
    proc FrameListEnsurePopulated {tbl row} {
        variable StackFrameHeaders
        set RowName [$tbl rowcget $row -name]
        if {[regexp {^fr[0-9]*$} $RowName]} {
            set i [GetStackFrameHeaderIndexByRowName $RowName]
            set item [lindex $StackFrameHeaders $i]
            GetAndInsertLocals $tbl $RowName
        }
    }

    # Insert stack frame into StackFrameHeaders
    # variable and into tablelist. 
    # contents - text to display
    # type - irrelevant
    # FrameNo - number of frame
    # Adds item to StackFrameHeaders:
    proc AppendData {contents type FrameNo} {
        variable DbgMainWindow
        variable StackFrameHeaders

        if {!([info exists DbgMainWindow]&&[winfo exists $DbgMainWindow])} {
            return
        }

        set RowName [FrameNumberToRowName $FrameNo]
        
        set NewItem [dict create            \
                         contents $contents         \
                         type $type         \
                         FrameNo $FrameNo   \
                         RowName $RowName   \
                        ]
        
        lappend StackFrameHeaders $NewItem

        set tbl $DbgMainWindow.tf.tbl    
        set row [$tbl insertchild root end [list $contents]]

        $tbl rowconfigure $row -name $RowName
        $tbl collapse $row
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
            set FrameNo [::mprs::Unleash [lindex $frame 0]]
            set contents [::mprs::Unleash [lindex $frame 1]]
            set type 0
            AppendData $contents $type $FrameNo
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

    # number - is a serial number of a local amongst locals of that frame
    # Example of Local:
    # :name snumber :id n0 :value s0
    proc InsertLocalIntoTree {tbl ParentRowName number Local} {
        puts $Local
        set varname [::mprs::Unleash [dict get $Local {:name}]]
        set varvalue [::mprs::Unleash [dict get $Local {:value}]]
        set contents "$varname = $varvalue"
        set row [$tbl insertchildren $ParentRowName end [list $contents]]
        set name [string cat $ParentRowName "_" $number]
        $tbl rowconfigure $row -name $name
    }

    proc InsertLocalsForFrameIntoTree {RowName EventAsList} {
        variable DbgMainWindow
        if {[info exists DbgMainWindow]&&[winfo exists $DbgMainWindow]} {
            set tbl [::ldbg::GetDbgMainWindowMenuTbl $DbgMainWindow]
            $tbl delete [$tbl childkeys $RowName]

            set okList [::mprs::Unleash [lindex $EventAsList 1]]
            if {[::mprs::Unleash [lindex $okList 0]] ne {:ok}} {
                error "Something wrong with the debugger: error showing locals"
            }
            set LocalsAndX [::mprs::Unleash [lindex $okList 1]]
            set Locals [::mprs::Unleash [lindex $LocalsAndX 0]]
            set i 0
            foreach Local $Locals {
                InsertLocalIntoTree $tbl $RowName $i [::mprs::Unleash $Local]
                incr i
            }
            puts "What is second LocalsAndX? See slimv"        
        }
    }
    
    proc GetAndInsertLocals {tbl RowName} {
        variable DbgMainWindow
        set grabber [TitleOfErrorBrowser $DbgMainWindow]
        set FrameNo [RowNameToFrameNo $RowName]

        
        #set OnReply "::ldbg::InsertLocalsForFrameIntoTree $RowName \$EventAsList; grab release $grabber"
        # grab $grabber

        set OnReply "::ldbg::InsertLocalsForFrameIntoTree $RowName \$EventAsList"
        ::tkcon::EvalInSwankAsync \
            "(swank:frame-locals-and-catch-tags $FrameNo)" \
            $OnReply 0 [GetDebuggerThreadId]
    }

    proc CellCmd {row action} {
        variable ::edt::EditorMRUWinList
        variable DbgMainWindow
        set p [lindex $EditorMRUWinList $row]
        set tbl [GetDbgMainWindowMenuTbl $DbgMainWindow]
        set RowName [$tbl rowcget $row -name]
        switch -exact $action {
            GetAndInsertLocals {
                GetAndInsertLocals $tbl $RowName
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

    # FIXME - this must be a code where e2 is a hyperlink to condition inspector
    proc WriteDebuggerTitle {w} {
        variable DebugEvent
        set TitleListL [lindex $DebugEvent 3]
        if {![::mprs::Consp $TitleListL]} {
            error "I expected a cons: $TitleListL"
        }
        set TitleList [::mprs::Unleash $TitleListL]
        set e1 [::mprs::Unleash [lindex $TitleList 0]]
        set e2 [::mprs::Unleash [lindex $TitleList 1]]
        ::tkcon::WritePassiveText $w "$e1\n" end
        ::tkcon::WriteActiveText $w $e2 end "tk_messageBox -message {inspect condition}"
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

        $w.title.text RoDelete 0.0 end
        WriteDebuggerTitle $w.title.text

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

    proc DbgMainWindowEditMenu {w menu} {
        set m [menu [::tkcon::MenuButton $menu "2.Edit" edit]]
        set tbl [GetDbgMainWindowMenuTbl $w ]
        set bodytag [$tbl bodytag]
        set cmd "::tablelist_util::CopyCurrentCell $tbl"
	$m add command -label "Copy"  -accel "Control-C" \
            -command $cmd

        
        bind $bodytag <Control-Key-c> $cmd
        bind $bodytag <Control-Key-Cyrillic_es> $cmd
        bind $bodytag <Control-Key-Insert> $cmd
        
        set cmd [list ::fndrpl::OpenFindBox $tbl "tablelist" "find" "::ldbg::FrameListEnsurePopulated"]
	$m add command -label "Find"  -accel "Control-f" \
            -command $cmd
        
        bind $bodytag <Control-Key-f>	   $cmd
        bind $bodytag <Control-Key-Cyrillic_a> $cmd
        

        set cmd [list ::fndrpl::FindIt $tbl]
	$m add command -label "Find again"  -underline 0 -accel "F3" -command $cmd -state disabled
        #bind $tbl <F3> $cmd
        
    
        # set cmd ::srchtblst::
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

    # Responce to :debug-activate, stolen from slimv
    proc DebugActivate { EventAsList } {
        puts "DebugActivate may go wrong"
        variable ::slimv::debug_active
        variable ::slimv::debug_activated
        variable ::slimv::current_thread
        variable ::slimv::Ssldb_level
        set ::slimv::debug_active 1
        set ::slimv::debug_activated 1
        set ::slimv::current_thread [::mprs::Unleash [lindex $EventAsList 1]]
        set ::slimv::Ssldb_level [::mprs::Unleash [lindex $EventAsList 2]]
        set ::slimv::frame_locals [dict create]
        return ''
    }


    # See swank::sldb-loop to identify interface
    # swank -> tcl : debug
    # swank -> tcl : debug-activate
    # loop
    #   wait for swank <- tcl: either emacs-rex (evaluate it)
    #   or for sldb-return
    #   if sldb-return then return from loop
    #     conditions in sldb are just reported
    # swank -> tcl debug-return
    # tcl -> swank sldb-return
    # when (> level 1)
    #  (send-event (current-thread) `(:sldb-return ,level))

    # Responce to :debug-return, stolen from slimv and melted with our
    # normal continuation-based responce processing 
    proc DebugReturn { EventAsList ContinuationId } {
        puts "DebugReturn may go wrong"
        variable ::slimv::debug_active
        variable ::slimv::Ssldb_level
        #vim.command('let s:sldb_level=-1')
        #retval = retval + '; Quit to level ' + r[2] + '\n' + get_prompt()
        set level [::mprs::Unleash [lindex $EventAsList 2]]
        puts "; Quit to level $level"
        set ::slimv::Ssldb_level $level
        if {$ContinuationId ne ""} {
            ::mprs::RunContinuation $ContinuationId $EventAsList
        }
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

    proc Abort {w} {
        variable DebugEvent
        variable Restarts
        variable DbgMainWindow
        set thread [GetDebuggerThreadId]
        set level [GetDebuggerLevel]
        ::tkcon::EvalInSwankAsync \
            "(swank:sldb-abort)" \
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
            set Restart [::mprs::Unleash $RestartL]
            set Short [::mprs::Unleash [lindex $Restart 0]]
            set Long [::mprs::Unleash [lindex $Restart 1]]
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
            set tbl [GetDbgMainWindowMenuTbl $DbgMainWindow]
            $tbl delete 0 end
            #$tbl insertchildlist root end "tree"
            #$tbl rowconfigure 0 -name "TreeRoot"
            #$tbl collapse 0
        }
    }

    proc MakeBindings {w} {
        set tbl [GetDbgMainWindowMenuTbl $w]
        set bodytag [$tbl bodytag]
        
        # wcb::callback $tbl before activate ::ldbg::DoOnSelect
        #bind $bodytag <space> {::ldbg::KbdCellCmd %W %x %y GetAndInsertLocals; break}
        #bind $bodytag <Return> {::ldbg::KbdCellCmd %W %x %y HideListAndShowBuffer; break}
        #bind $bodytag <Delete> {::ldbg::KbdCellCmd %W %x %y CloseBuffer; break}
        #bind $bodytag <Double-Button-1> {::ldbg::MouseCellCmd %W %x %y GetAndInsertLocals; break}
        
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
        set w $PRIV(base).ldbgTlv
        if {[winfo exists $w]} {
            ClearStackFramesTableList
            return $w
        }

        set metrics [font measure tkconfixed "w"]
        toplevel $w -width [expr { 50 * $metrics }]
        wm withdraw $w

     
        # title
        set level [GetDebuggerLevel]
        set word "Debugger level $level"
        wm title $w $word

        set DbgMainWindow $w

        # --------------- frames, tablelist -----------------
        # --------------- title (condition description) ------
        frame $w.title
        text $w.title.text -height 6
        InitTextReadonly $w.title.text 1
        scrollbar $w.title.sx -orient h -command [list $w.title.text xview]
        scrollbar $w.title.sy -orient v -command [list $w.title.text yview]
        ::insp::ConfigureTextFonts $w.title.text
        $w.title.text configure \
            -xscrollcommand [list $w.title.sx set] \
            -yscrollcommand [list $w.title.sy set] 


        # --------------- stack frames list -----------------
        frame $w.tf
        set tbl $w.tf.tbl
        
        tablelist::tablelist $tbl \
            -columns {60 "Frame" left} \
            -stretch 0 -spacing 10 \
            -width 62   \
            -expandcommand "::ldbg::FrameListEnsurePopulated"
        
        $tbl resetsortinfo 

        $tbl configure \
            -foreground \#000000 \
            -font tkconfixed -borderwidth 1 -highlightthickness 1 
        

        $tbl columnconfigure 0 -wrap true  

        # ----------------------------------- menu bar -------------------
        
        set menu [menu $w.mbar]
        $w configure -menu $menu
        # Menu items created later as they refer to window contents
        
        # TitleListFileMenu $w $menu
        DbgMainWindowEditMenu $w $menu
        DbgMainWindowWindowMenu $w $menu
        DbgMainWindowRestartsMenu $w $menu

        # ------------------------------ bindings -------------
        MakeBindings $w
        wm protocol $w WM_DELETE_WINDOW "::ldbg::Abort $w"
        # -------------------------- layout -------------------
        
        # now layout title elements in title
        grid $w.title.text - $w.title.sy -sticky news
        grid $w.title.sx - -sticky ew
        grid columnconfigure $w.title 0 -weight 1
        grid columnconfigure $w.title 1 -weight 1
        grid rowconfigure $w.title 0 -weight 1

        # stack frames list layout
        set f1 $w.tf
        scrollbar $f1.sy -orient v -command [list $tbl yview]
        $tbl configure -yscrollcommand [list $f1.sy set]
        grid $tbl - $f1.sy -sticky news
        grid columnconfigure $f1 0 -weight 1
        grid columnconfigure $f1 1 -weight 0
        grid rowconfigure $f1 0 -weight 1

        pack $w.title -side top -fill x
        pack $f1 -fill both -expand 1

        $tbl selection anchor 0

        return $w    
    }

    # Returns tablelist by main error browser window
    proc GetDbgMainWindowMenuTbl {w} {
        return $w.tf.tbl
    }

    proc TitleOfErrorBrowser {w} {
        set f1 $w.title
        return $w.title.text
    }
}

# ::buli::DebugStartup
