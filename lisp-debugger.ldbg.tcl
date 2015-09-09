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
    
    variable MainWindow

    # dictionary $FrameNo -> Item, see 
    variable StackFrameHeaders

    # dictionary of StackFrameHeaders being filled.
    # dictionary $FrameNo -> [list of continuation bodies to call after filling]
    variable StackFrameHeadersBeingFilled 

    catch {font create tkconfixed -family Courier -size -20}

    proc InitData {} {
        variable StackFrameHeaders
        variable StackFrameHeadersBeingFilled
        set StackFrameHeaders [dict create]
        set StackFrameHeadersBeingFilled  [dict create]
    }

    # Creates a tablelist -name row option value for a frame number
    proc FrameNumberToRowName {n} {
        string cat "fr" $n
    }

    # Args:
    # RowName - -name option from tablelist row
    # Returns:
    # index of item in StackFrameHeaders with that RowName
    proc RowNameToFrameNo {RowName} {
        variable StackFrameHeaders
        dict for {frameNo item} $StackFrameHeaders {
            if {[dict get $item RowName] eq $RowName} {
                return $frameNo
            }
        }
        error "RowNameToFrameNo: failed at $RowName"
    }

    # Get data from StackFrameHeaders associated with the row.
    # Args: tablelist and row key in any form
    # Result: 
    # list of the following elements:
    #  - kind of the frame:
    #     -- Local - it is a local
    #     -- Frame - it is a frame
    #     -- CatchTag - it is a catch tag
    #     -- Nothing - separator, root, etc
    #  - for a local - number of the local
    #  - item of the row
    #  - parent item of the row if appropriate
    # if it is a local,
    # "CatchTag" if it is a catch tag
    proc RowToItemInfo {tbl row} {
        variable StackFrameHeaders
        set RowName [$tbl rowcget $row -name]
        set path [regexp -all -inline {[[:alnum:]]+} $RowName]
        while {1} {
            # this is a tagbody
            set pathLen [llength $path]
            if {$pathLen == 0} break
            set a1 [lindex $path 0]
            set frameNo {}
            regexp {^fr([0-9]+)$} $a1 dummy frameNo
            if {$frameNo eq {}} break
            set frameItem [dict get $StackFrameHeaders $frameNo]
            if {$pathLen == 1} {
                return [list "Frame" $frameItem]
            }
            set a2 [lindex $path 1]
            set x2type {}
            set x2no {}
            regexp {^([a-z]+)([0-9]+)$} $a2 dummy x2type x2no
            switch -exact $x2type {
                "lo" {
                    set localItem [dict get $frameItem "Locals" $x2no]
                    return [list "Local" $x2no $localItem $frameItem]
                }
                "ct" {
                    error "Wow! I'm not written!"
                }
                default {
                    break
                }
            }
        }
        error "wrong RowName $RowName"
    }

    proc FramesTablelistEnsurePopulated {tbl row {contBody ProcedureNop}} {
        variable StackFrameHeaders
        set RowName [$tbl rowcget $row -name]
        if {[regexp {^fr[0-9]*$} $RowName]} {
            GetAndInsertLocsNTags $tbl $RowName $contBody 
        } else {
            set lambda [list {} $contBody]
            apply $lambda
        }
    }

    # Insert stack frame into StackFrameHeaders
    # variable and into tablelist. 
    # contents - text to display
    # type - irrelevant
    # FrameNo - number of frame
    # Adds item to StackFrameHeaders:
    proc InsertFrameIntoTree {contents type FrameNo} {
        variable MainWindow
        variable StackFrameHeaders
        variable ::tkcon::COLOR

        if {!([info exists MainWindow]&&[winfo exists $MainWindow])} {
            return
        }

        set RowName [FrameNumberToRowName $FrameNo]
        
        set NewItem [dict create                    \
                         contents $contents         \
                         type $type                 \
                         FrameNo $FrameNo           \
                         RowName $RowName           \
                         Locals [dict create]       \
                         CatchTags [dict create]    \
                        ]
        
        dict set StackFrameHeaders $FrameNo $NewItem

        set tbl $MainWindow.tf.tbl
        set prefix [format "%2d" $FrameNo]
        set row [$tbl insertchild root end [list [string cat $prefix ": " $contents]]]

        $tbl rowconfigure $row -name $RowName -background $::tkcon::COLOR(ldbg_frame_bg)
        $tbl collapse $row
    }

    proc InsertSeveralFramesIntoTree { frames } {
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
            InsertFrameIntoTree $contents $type $FrameNo
        }
    }

    # localNo - is a zero-based serial number of a local amongst locals of that frame
    # Example of Local:
    # :name snumber :id n0 :value s0
    # local will have RowName = frNN_loMM, where NN is frameNo, MM is localNo
    # See also: InsertCatchTagIntoTree
    proc InsertLocalIntoTree {tbl ParentRowName localNo Local} {
        variable StackFrameHeaders
        set ParentFrameNo [RowNameToFrameNo $ParentRowName]
        dict set StackFrameHeaders $ParentFrameNo "Locals" $localNo $Local
        set varname [::mprs::Unleash [dict get $Local {:name}]]
        set varvalue [::mprs::Unleash [dict get $Local {:value}]]
        set contents "$varname = $varvalue"
        set row [$tbl insertchildren $ParentRowName end [list $contents]]
        set name [string cat $ParentRowName "_lo" $localNo]
        $tbl rowconfigure $row -name $name
    }

    # LocalsL is a leashed list of locals
    proc InsertLocalsOfOneFrameIntoTree {tbl FrameRowName LocalsL} {
        if {[::mprs::Consp $LocalsL]} {
            set Locals [::mprs::Unleash $LocalsL]
            set i 0
            foreach LocalL $Locals {
                set Local [::mprs::Unleash $LocalL]
                InsertLocalIntoTree $tbl $FrameRowName $i $Local
                incr i
            }
        }
    }

    # Simplified clone of InsertLocalIntoTree
    # Args: CatchTag is an unleashed catch tag name
    # There seem to be a bug in swank: catch tag is not qualified with package name? 
    proc InsertCatchTagIntoTree {tbl ParentRowName catchTagNo CatchTag} {
        variable StackFrameHeaders
        set ParentFrameNo [RowNameToFrameNo $ParentRowName]
        dict set StackFrameHeaders $ParentFrameNo "CatchTags" $catchTagNo $CatchTag
        set contents "\[Catch tag\] $CatchTag"
        set row [$tbl insertchildren $ParentRowName end [list $contents]]
        set name [string cat $ParentRowName "_ct" $catchTagNo]
        $tbl rowconfigure $row -name $name
    }
    
    proc InsertCatchTagsOfOneFrameIntoTree {tbl FrameRowName CatchTagsL} {
        if {[::mprs::Consp $CatchTagsL]} {
            set CatchTags [::mprs::Unleash $CatchTagsL]
            set i 0
            foreach CatchTagL $CatchTags {
                # CatchTag is just a string
                set CatchTag [::mprs::Unleash $CatchTagL]
                InsertCatchTagIntoTree $tbl $FrameRowName $i $CatchTag
                incr i
            }
        }
    }

    proc InsertLocsNTagsForFrameIntoTree {RowName EventAsList} {
        variable MainWindow
        variable StackFrameHeadersBeingFilled
        if {[info exists MainWindow]&&[winfo exists $MainWindow]} {
            set tbl [::ldbg::GetFramesTablelist $MainWindow]
            if {![llength [$tbl childkeys $RowName]]} {
                set okList [::mprs::Unleash [lindex $EventAsList 1]]
                if {[::mprs::Unleash [lindex $okList 0]] ne {:ok}} {
                    error "Something wrong with the debugger: error showing locals"
                }
                # lact =  LocalsAndCatchTags
                set lact [::mprs::Unleash [lindex $okList 1]]
                set LocalsL [lindex $lact 0]
                InsertLocalsOfOneFrameIntoTree $tbl $RowName $LocalsL
                set CatchTagsL [lindex $lact 1]
                InsertCatchTagsOfOneFrameIntoTree $tbl $RowName $CatchTagsL
            }
            set FrameNo [RowNameToFrameNo $RowName]
            if {[dict exists $StackFrameHeadersBeingFilled $FrameNo]} {
                set conts [dict get $StackFrameHeadersBeingFilled $FrameNo]
                dict unset StackFrameHeadersBeingFilled $FrameNo
                foreach cont $conts {
                    set lambda [list {} $cont]
                    apply $lambda
                }
            }
        }
    }


    # contBody is a body of a parameterless continuation
    proc GetAndInsertLocsNTags {tbl RowName contBody} {
        variable MainWindow
        variable StackFrameHeadersBeingFilled
        # puts "Entered GetAndInsertLocsNTags"
        set grabber [GetTitleTextWidget $MainWindow]
        set FrameNo [RowNameToFrameNo $RowName]

        # If we now are filling already, post our continuation after
        # continuations which were sheduled already
        # Note that order of events can change, but at least
        # we will not lose any continuation
        if {[dict exists $StackFrameHeadersBeingFilled $FrameNo]} {
            set curvalue [dict get $StackFrameHeadersBeingFilled $FrameNo]
            set newvalue [lappend curvalue $contBody]
            dict set StackFrameHeadersBeingFilled $FrameNo $newvalue
            # Do nothing 
            return
            #
        } else {
            dict append StackFrameHeadersBeingFilled $FrameNo [list $contBody]
        }

        set OnReply "::ldbg::InsertLocsNTagsForFrameIntoTree $RowName \$EventAsList"
        ::tkcon::EvalInSwankAsync \
            "(swank:frame-locals-and-catch-tags $FrameNo)" \
            $OnReply 0 [GetDebuggerThreadId]
    }


    proc EditFrameSource {tbl RowName} {
        set FrameNo [RowNameToFrameNo $RowName]
        set TblForLisp [::tkcon::QuoteLispObjToString $tbl]
        ::tkcon::EvalInSwankAsync                                                   \
            "(clcon-server:ldbg-edit-frame-source-location $FrameNo $TblForLisp)"   \
            {} 0 [GetDebuggerThreadId]
    }
    
    proc RowDblClick {tbl RowName} {
        set ItemInfo [RowToItemInfo $tbl $RowName]
        set type [lindex $ItemInfo 0]
        switch -exact $type {
            "Local" {
                LocalInspectValue $tbl $RowName
            }
            "Frame" {
                EditFrameSource $tbl $RowName
            }
        } 
    }            
    
    proc CellCmd {row action} {
        variable ::edt::EditorMRUWinList
        variable MainWindow
        set p [lindex $EditorMRUWinList $row]
        set tbl [GetFramesTablelist $MainWindow]
        set RowName [$tbl rowcget $row -name]
        switch -exact $action {
            GetAndInsertLocsNTags {
                GetAndInsertLocsNTags $tbl $RowName
            }
            RowDblClick {
                RowDblClick $tbl $RowName
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
        ::tkcon::WriteActiveText $w $e2 end "::ldbg::InspectCurrentCondition"
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
        InsertSeveralFramesIntoTree $frames
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

    proc MakeMainWindowStackMenu {w menu} {
        set m [menu [::tkcon::MenuButton $menu "1.Stack" stack]]
        set tbl [GetFramesTablelist $w ]
        set cmd "::ldbg::CellCmdForActiveCell $tbl RowDblClick"
        $m add command -label "Edit source/inspect local" -accel "Return" -command $cmd
        #
        set cmd "::ldbg::InspectCurrentCondition"
        $m add command -label "1.Inspect current condition" -underline 0 -command $cmd
    }
    
    proc MakeMainWindowEditMenu {w menu} {
        set m [menu [::tkcon::MenuButton $menu "2.Edit" edit]]
        set tbl [GetFramesTablelist $w ]
        set bodytag [$tbl bodytag]
        set cmd "::tablelist_util::CopyCurrentCell $tbl"
	$m add command -label "Copy"  -accel "Control-C" \
            -command $cmd
        
        bind $bodytag <Control-Key-c> $cmd
        bind $bodytag <Control-Key-Cyrillic_es> $cmd
        bind $bodytag <Control-Key-Insert> $cmd
        
        set cmd [list ::fndrpl::OpenFindBox $tbl "tablelist" "find" "::ldbg::FramesTablelistEnsurePopulated"]
	$m add command -label "Find"  -accel "Control-f" \
            -command $cmd
        
        bind $bodytag <Control-Key-f>	   $cmd
        bind $bodytag <Control-Key-Cyrillic_a> $cmd
        

        set cmd [list ::fndrpl::FindIt $tbl]
	$m add command -label "Find again"  -underline 0 -accel "F3" -command $cmd -state disabled
        #bind $tbl <F3> $cmd
        
    
        # set cmd ::srchtblst::
    }
    
    proc MakeMainWindowWindowMenu {w menu} {
        ## Window Menu
        ##
        set m [menu [::tkcon::MenuButton $menu "7.Window" window]]
        set cmd [list ::clconcmd::bufferlist]
	$m add command -label "Buffer list" -accel "Control-F12" \
            -command $cmd 
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

    # Invokes inspector on local variable value
    # Args: RowName of variable entry
    proc LocalInspectValue {tbl RowName} {
        set ItemInfo [RowToItemInfo $tbl $RowName]
        set type [lindex $ItemInfo 0]
        if {$type ne "Local"} {
            error "LocalInspectValue: not a local"
        }
        set LocalNo [lindex $ItemInfo 1]
        set LocalItem [lindex $ItemInfo 2]
        set FrameItem [lindex $ItemInfo 3]
        set FrameNo [dict get $FrameItem "FrameNo"]
        set thread [GetDebuggerThreadId]
        #
        ::tkcon::EvalInSwankAsync                           \
            "(swank:inspect-frame-var $FrameNo $LocalNo)"   \
            "::insp::SwankInspect1 \$EventAsList"           \
            0 $thread
    }

    proc InspectCurrentCondition {} {
        set thread [GetDebuggerThreadId]                    
        ::tkcon::EvalInSwankAsync                           \
            " (swank:inspect-current-condition)"            \
            "::insp::SwankInspect1 \$EventAsList"           \
            0 $thread
    }      
    
    # (:emacs-rex
    #  (swank:invoke-nth-restart-for-emacs 1 2)
    #  "COMMON-LISP-USER" 17 33)

    proc InvokeRestart {i} {
        variable DebugEvent
        variable Restarts
        variable MainWindow
        set thread [GetDebuggerThreadId]
        set level [GetDebuggerLevel]
        ::tkcon::EvalInSwankAsync \
            "(swank:invoke-nth-restart-for-emacs $level $i)" \
            {} 0 $thread
        after idle destroy $MainWindow
    }

    proc Abort {w} {
        variable DebugEvent
        variable Restarts
        variable MainWindow
        set thread [GetDebuggerThreadId]
        set level [GetDebuggerLevel]
        ::tkcon::EvalInSwankAsync \
            "(swank:sldb-abort)" \
            {} 0 $thread
        after idle destroy $MainWindow
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
    
    proc MakeMainWindowRestartsMenu {w menu} {
        variable ::tkcon::COLOR
        set m [::tkcon::MenuButton $menu "4.Restarts" restarts]
        menu $m -disabledforeground $::tkcon::COLOR(disabled) \
            -postcommand [list ::ldbg::FillRestartsMenu $m]
    }

    proc ClearStackFramesTableList {} {
        variable MainWindow
        if {[winfo exists $MainWindow]} {
            set tbl [GetFramesTablelist $MainWindow]
            $tbl delete 0 end
            #$tbl insertchildlist root end "tree"
            #$tbl rowconfigure 0 -name "TreeRoot"
            #$tbl collapse 0
        }
    }

    proc DoOnSelect {tbl row} {
        #puts [RowToItemInfo $tbl $row]
    }


    proc MakeBindings {w} {
        set tbl [GetFramesTablelist $w]
        set bodytag [$tbl bodytag]
        
        wcb::callback $tbl before activate ::ldbg::DoOnSelect
        #bind $bodytag <space> {::ldbg::KbdCellCmd %W %x %y GetAndInsertLocals; break}
        bind $bodytag <Return> {::ldbg::KbdCellCmd %W %x %y RowDblClick; break}
        #bind $bodytag <Delete> {::ldbg::KbdCellCmd %W %x %y CloseBuffer; break}
        bind $bodytag <Double-Button-1> {::ldbg::MouseCellCmd %W %x %y RowDblClick; break}
        
        #    bind $w.tf.tbl <<TablelistCellUpdated>> [list DoOnSelect $w.tf.tbl]
        #    bind $w.tf.tbl <<ListBoxSelect>> [list DoOnSelect $w.tf.tbl]
    }

    
    # Make toplevel widget and its children
    # Returns window
    proc PrepareGui1 {} {
        variable MainWindow

        # ---------------------------- make toplevel window MainWindow -----------    
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

        set MainWindow $w

        # --------------- frames, tablelist -----------------
        # --------------- title (condition description) ------
        frame $w.title
        ::clcon_text::clcon_text $w.title.text -height 6 -readonly 1
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
            -expandcommand "::ldbg::FramesTablelistEnsurePopulated"
        
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
        MakeMainWindowStackMenu $w $menu
        MakeMainWindowEditMenu $w $menu
        MakeMainWindowWindowMenu $w $menu
        MakeMainWindowRestartsMenu $w $menu

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

        #::tablelist_util::TreeSetTo $tbl 0

        return $w    
    }

    # Returns tablelist where frames live
    proc GetFramesTablelist {w} {
        return $w.tf.tbl
    }

    proc GetTitleTextWidget {w} {
        set f1 $w.title
        return $w.title.text
    }
}

# ::buli::DebugStartup
