## Debugger
# TkconSourceHere search-tablelist.tcl

namespace eval ::ldbg {

    # Initial event which caused entering into a debugger as lisdt
    variable DebugEvent

    # list of leashed lists of two elements - short and long name of a restart
    variable Restarts

    # we try to keep this var 1 when debugger is active and 0 when it is inactive
    defvar InTheDebugger 1
    # stepper mode is 1 when we are in stepper
    defvar StepperMode 0
    
    # We delete MainWindow when reloading source so that it will be refreshed
    # This is appropriate for some tools only. E.g. rebuilding editor is not
    # a good idea.
    variable MainWindow {}

    
    variable StepperMenuPathname 

    # dictionary $FrameNo -> Item, see 
    variable StackFrameHeaders

    # dictionary of StackFrameHeaders being filled.
    # dictionary $FrameNo -> [list of continuation bodies to call after filling]
    variable StackFrameHeadersBeingFilled 

    # stores a location of debugger window (for current session only)
    variable LispDebuggerGeometry 

    catch {font create tkconfixed -family Courier -size -20}

    proc StepperMenuTitle {} {
        return "Stepper"
    }

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

        # apply - canonical example of metaprogramming
        # We use subst to take care of vars from current scope
        # Vars which will be available at evaluation time are protected
        # escaping \$
        set lispCmd "(swank:frame-locals-and-catch-tags $FrameNo)"
        ::tkcon::EvalInSwankAsync $lispCmd [subst -nocommands {
            ::ldbg::InsertLocsNTagsForFrameIntoTree $RowName \$EventAsList
        }] [GetDebuggerThreadId]
    }


    proc EditFrameSource {tbl RowName} {
        set FrameNo [RowNameToFrameNo $RowName]
        set TblForLisp [::tkcon::QuoteLispObjToString $tbl]
        ::tkcon::EvalInSwankAsync                                                   \
            "(clcon-server:ldbg-edit-frame-source-location $FrameNo $TblForLisp)"   \
            {} [GetDebuggerThreadId]
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
        variable MainWindow
        set tbl [GetFramesTablelist $MainWindow]
        set RowName [$tbl rowcget $row -name]
        switch -exact $action {
            GetAndInsertLocsNTags {
                GetAndInsertLocsNTags $tbl $RowName
            }
            RowDblClick {
                RowDblClick $tbl $RowName
            }
            ReturnFromFrame {
                ReturnFromFrame $RowName
            }
            EvalInFrame {
                EvalInFrame $RowName
            }
            EvalInFramePrettyPrint {
                EvalInFramePrettyPrint $RowName
            }
            EnableStepping {
                EnableStepping
            }
            #SwitchToNativeDebugger {
            #    SwitchToNativeDebugger $RowName
            #}    
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

    proc EnableDisableMenus { DbgToplevelWindow } {
        variable StepperMode
        variable StepperMenuPathname
        if {$StepperMode} {
            set state normal
        } else {
            set state disabled
        }
        # showVar StepperMenuPathname
        
        .ldbgTlv.mbar entryconfigure [StepperMenuTitle] -state $state
    }
    
    # This is a contiuation assigned on reply on initialization request 
    proc ldbg { EventAsList } {
        variable DebugEvent
        variable Restarts
        variable StepperMode
        variable LispDebuggerGeometry
        variable InTheDebugger

        set DebugEvent $EventAsList

        set rasm [ParseRestarts]
        set Restarts [lindex $rasm 0]
        set StepperMode [lindex $rasm 1]
        
        InitData

        set w [PrepareGui1]

        set tbl $w.tf.tbl

        $w.title.text RoDelete 1.0 end
        WriteDebuggerTitle $w.title.text

        set frames [ExtractStackFrames $EventAsList]
        InsertSeveralFramesIntoTree $frames
        #HighlightCurrentlyVisibleBuffer

        EnableDisableMenus $w

        set InTheDebugger 1
        DoGoToTop $w
        
        # if {[info exists LispDebuggerGeometry]} {
        #     showVar LispDebuggerGeometry
        #     wm geometry $w $LispDebuggerGeometry
        # }

        if {$StepperMode} {
            # In a stepper mode, expand locals at topmost stack frame
            $tbl expand 0
            # In a stepper mode, show source immediately
            CellCmd 0 RowDblClick
        } else {
            focus $tbl
        }
        
        return
    }

    proc DoGoToTop {w} {
        wm deiconify $w
        raise $w
    }

    proc CellCmdForActiveCell {tbl Cmd} {
        variable MainWindow
        set active [$tbl index active]
        if {$active eq {}} {
            tk_messageBox -parent $MainWindow -title "Action for active cell" -message "No active cell"
            return
        }
        CellCmd $active $Cmd
    }

    proc MakeMainWindowStackMenu {w menu} {
        set m [menu [::tkcon::MenuButton $menu "1.Stack" stack]]
        set tbl [GetFramesTablelist $w ]

        # FIXME redo all that strings to lists as with EnableStepping
        set cmd "::ldbg::CellCmdForActiveCell $tbl RowDblClick"
        $m add command -label "Edit source/inspect local" -accel "Return" -command $cmd
        #
        set cmd "::ldbg::InspectCurrentCondition"
        $m add command -label "1.Inspect current condition" -underline 0 -command $cmd

        set cmd "::ldbg::CellCmdForActiveCell $tbl ReturnFromFrame"
        $m add command -label "2.Return from frame" -underline 0 -command $cmd

        set cmd "::ldbg::CellCmdForActiveCell $tbl EvalInFrame"
        $m add command -label "3.Eval in frame" -underline 0 -command $cmd

        set cmd "::ldbg::CellCmdForActiveCell $tbl EvalInFramePrettyPrint"
        $m add command -label "4.Eval in frame (pretty print)" -underline 0 -command $cmd

        set cmd [list ::ldbg::CellCmdForActiveCell $tbl EnableStepping]
        $m add command -label "5.Switch to stepping mode" -underline 0 -command $cmd        
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
        set RestartsL [lindex $DebugEvent 4]
        if {![::mprs::Consp $RestartsL]} {
            error "Expected cons: $RestartsL"
        }
        set restarts [::mprs::Unleash $RestartsL]
        set stepperMode 0

        foreach r $restarts {
            set restartName [::mprs::Unleash [::mprs::Car $r]]
            switch -nocase $restartName {
                step-continue {
                    set stepperMode 1
                    break
                }
            }
        }
        list $restarts $stepperMode
    }

    # Responce to :debug-activate, stolen from slimv
    proc DebugActivate { EventAsList } {
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
            $thread
    }

    proc InspectCurrentCondition {} {
        set thread [GetDebuggerThreadId]                    
        ::tkcon::EvalInSwankAsync                           \
            " (swank:inspect-current-condition)"            \
            "::insp::SwankInspect1 \$EventAsList"           \
            $thread
    }

    proc EnableStepping {} {
        set thread [GetDebuggerThreadId]                    
        ::tkcon::EvalInSwankAsync                           \
            " (swank:sldb-step 0)"                          \
            {}                                              \
            $thread
    }

    
    #(:emacs-rex
    #(swank:sldb-return-from-frame 1 "(values nil nil)")
    #"COMMON-LISP-USER" 2 21)
    # (:return
    #(:abort "nil")
    # 21)
    # (:debug-return 2 1 nil)

    # Remove debugger window from the screen and sets InTheDebugger to 0
    # FIXME rename to CloseDebugger
    proc CloseDebuggerWindow {MainWindow} {
        variable InTheDebugger 0
        # We keep window so that it could recall its size and position.
        wm withdraw $MainWindow
    }
    
    proc ReturnFromFrame {RowName} {
        variable MainWindow
        set FrameNo [RowNameToFrameNo $RowName]
        #set level [GetDebuggerLevel]
        set thread [GetDebuggerThreadId]
        foreach {isok code}                                 \
            [ LameAskForLispExpression $MainWindow          \
                  "Enter lisp expr (shift-enter=new line) and press Enter: cl-user>"
             ] break
        if {$isok ne "ok"} {
            return
        }
        set qCode [::tkcon::QuoteLispObjToString $code]
        ::tkcon::EvalInSwankAsync \
            "(swank:sldb-return-from-frame $FrameNo $qCode)" \
            {} $thread
        ::ldbg::CloseDebuggerWindow $MainWindow
    }

#(:emacs-rex
#  (swank:frame-package-name 0)
#  "COMMON-LISP-USER" 11 24)
# (:return
#  (:ok "SB-KERNEL")
#  24)
# (:emacs-rex
#  (swank:eval-string-in-frame "123" 0 "SB-KERNEL")
#  "COMMON-LISP-USER" 11 25)
# (:return
#  (:ok "=> 123 (7 bits, #x7B, #o173, #b1111011)")
#  25)

    proc EvalInFrame {RowName} {
        EvalInFrameInner $RowName 0
    }

    proc EvalInFramePrettyPrint {RowName} {
        EvalInFrameInner $RowName 1
    }

    
    proc EvalInFrameInner {RowName PrettyPrint} {
        set FrameNo [RowNameToFrameNo $RowName]
        ::tkcon::EvalInSwankAsync                                 \
            "(swank:frame-package-name $FrameNo)"                 \
            [subst -nocommand {
                ::ldbg::EvalInFrameC1 $FrameNo $PrettyPrint \$EventAsList
            }] [GetDebuggerThreadId]
    }

    proc EvalInFrameC1 {FrameNo PrettyPrint EventAsList} {
        variable MainWindow
        set package [::mprs::ParseReturnOk $EventAsList]
        set qPackage [::tkcon::QuoteLispObjToString $package]

        #set level [GetDebuggerLevel]
        foreach {isok code}                                      \
            [LameAskForLispExpression $MainWindow                \
                 "Eval in frame (shift-enter=new line)$package>" \
                ] break
        if {$isok ne "ok"} {
            return
        }
        puts ";;Eval in frame $FrameNo:"
        puts ";;$package> $code"
        set qCode [::tkcon::QuoteLispObjToString $code]
        set thread [GetDebuggerThreadId]
        if {$PrettyPrint} {
            set LispFn "swank:pprint-eval-string-in-frame"
        } else {
            set LispFn "swank:eval-string-in-frame"
        }
        ::tkcon::EvalInSwankAsync \
            "($LispFn $qCode $FrameNo $qPackage)" \
            {::ldbg::EvalInFrameC2 $EventAsList} $thread
    }
        

    proc EvalInFrameC2 {EventAsList} {
        set result [::mprs::ParseReturnOk $EventAsList]
        ::tkcon::FocusConsole
        puts $result
    }

    # Does not work in SLIME for me. No sence to port. 
    #(:emacs-rex
    # (swank:sldb-break-with-default-debugger nil)
    # nil 20 163)
    #(:return
    # (:abort "#<swank::invoke-default-debugger {E0CE4F1}>")
    # 163)
    #(:debug-return 20 1 nil)
    #(:write-string "\n\ndebugger invoked on a DIVISION-BY-ZERO in thread\n#<THREAD \"new-repl-thread\" RUNNING {B90CE61}>:\n  arithmetic error DIVISION-BY-ZERO signalled\nOperation was SB-KERNEL::DIVISION, operands (1 0).\n")

    #proc SwitchToNativeDebugger {i} {
    #    set thread [GetDebuggerThreadId]
    #    ::tkcon::EvalInSwankAsync "(swank:sldb-break-with-default-debugger nil)" {} $thread
    #        ::ldbg::CloseDebuggerWindow $MainWindow
    #}

    proc InvokeRestart {i} {
        variable DebugEvent
        variable Restarts
        variable MainWindow
        set thread [GetDebuggerThreadId]
        set level [GetDebuggerLevel]
        ::tkcon::EvalInSwankAsync \
            "(swank:invoke-nth-restart-for-emacs $level $i)" \
            {} $thread
        ::ldbg::CloseDebuggerWindow $MainWindow
    }


    proc InvokeStepperRestart {name} {
        variable StepperMode
        variable InTheDebugger
        variable MainWindow
        if {!$InTheDebugger} {
            tk_messageBox -parent $MainWindow -title "Stepper" -message "Debugger is not active"
        } elseif {!$StepperMode} {
            tk_messageBox -parent $MainWindow -title "Stepper" -message "Stepper is not active"
        } else {
            InvokeSldbRestartByName $name
        }
    }
    
    # Name must be a readable qualified lisp symbol, e.g. sb-ext:step-out
    proc InvokeSldbRestartByName {name} {
        set thread [GetDebuggerThreadId]
        set LispCmd "(clco::restart-with-name-exists-p '$name)"
        ::tkcon::EvalInSwankAsync $LispCmd \
            [subst -nocommands {
                ::ldbg::InvokeSldbRestartByNameC1 $name \$EventAsList
            }] $thread
    }


    proc InvokeSldbRestartByNameC1 {name EventAsList} {
        variable MainWindow
        set thread [GetDebuggerThreadId]
        set existsp [::mprs::ParseReturnOk $EventAsList]
        if {$existsp} {
            set LispCmd "(clco::invoke-sldb-restart-by-name '$name)"
            ::tkcon::EvalInSwankAsync $LispCmd {} $thread
        } else {
            tk_messageBox -parent $MainWindow -title "Restart" -message "Restart $name not found"
        }
        # Note we close the window just now, not after idle, as this can be stepper restart
        ::ldbg::CloseDebuggerWindow $MainWindow
    }

    
    proc Abort {w} {
        variable DebugEvent
        variable Restarts
        variable MainWindow
        set thread [GetDebuggerThreadId]
        set level [GetDebuggerLevel]
        ::tkcon::EvalInSwankAsync \
            "(swank:sldb-abort)" \
            {} $thread
        ::ldbg::CloseDebuggerWindow $MainWindow
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

    # For menu m, adds stepper command with label starting named restart and
    # binds it to each of bindtags
    proc StepperMenuItem {m bindtags name label accel} {
        variable MainWindow
        set w $MainWindow
        set cmd [list ::ldbg::InvokeStepperRestart $name]
        set cmdBreak "$cmd; break"
        $m add command -label $name -command $cmd -accel $accel
        foreach bt $bindtags {
            bind $bt $accel $cmdBreak
        }
    }

    # m is a menu. It can be in other window!
    proc FillStepperMenu {m bindtags} {
        StepperMenuItem $m $bindtags "sb-ext:step-continue" "continue" <F5>
        StepperMenuItem $m $bindtags "sb-ext:step-out" "step out" <Shift-F11>
        StepperMenuItem $m $bindtags "sb-ext:step-next" "next" <F10>
        StepperMenuItem $m $bindtags "sb-ext:step-into" "step into" <F11>
    }
    
    proc MakeMainWindowStepperMenu {w menu} {
        variable StepperMenuPathname
        set tbl [GetFramesTablelist $w]
        set bodytag [$tbl bodytag]
        set m [menu [::tkcon::MenuButton $menu [StepperMenuTitle] stepper]]
        set StepperMenuPathname $m
        FillStepperMenu $m [list $tbl $bodytag]
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
        # bind $w <Configure> "gui_util::RememberPositionOnConfigureEvent $w %W %x %y %h %w ::ldbg::LispDebuggerGeometry"
        #    bind $w.tf.tbl <<TablelistCellUpdated>> [list DoOnSelect $w.tf.tbl]
        #    bind $w.tf.tbl <<ListBoxSelect>> [list DoOnSelect $w.tf.tbl]
    }

    
    # Make toplevel widget and its children
    # Returns window
    proc PrepareGui1 {} {
        variable MainWindow
        variable LispDebuggerGeometry
        variable StepperMode

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
        ::gui_util::ConfigureTextFonts $w.title.text
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

        # StepperMode is set in the caller fn.
        MakeMainWindowStepperMenu $w $menu

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
