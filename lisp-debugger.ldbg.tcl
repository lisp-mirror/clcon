## Debugger
# TkconSourceHere search-tablelist.tcl

# Reloading the sources will crash current debugging session
# Рестарт по умолчанию при закрытии окна - ищи в исходниках sldb-quit-restart
# ( swank:*sldb-quit-restart* )
# Мы хотим расширить семантику:

namespace eval ::ldbg {

    # Initial event which caused entering into a debugger as lisdt
    variable DebugEvent

    # list of leashed lists of two elements - short and long name of a restart
    variable Restarts

    # we try to keep this var 1 when debugger is active and 0 when it is inactive
    variable InTheDebugger 0
    # stepper mode is 1 when we are in stepper
    variable StepperMode 0
    
    # We delete MainWindow when reloading source so that it will be refreshed
    # This is appropriate for some tools only. E.g. rebuilding editor is not
    # a good idea.
    variable MainWindow 
    if {[info exists MainWindow] && [winfo exists $MainWindow]} {
        catch { destroy $MainWindow }
    }
    
    variable StepperMenuPathname 

    # dictionary $FrameNo -> Item, see 
    variable StackFrameHeaders

    variable FramesRead
    set FramesRead 30

    # dictionary of StackFrameHeaders being filled.
    # dictionary $FrameNo -> [list of continuation bodies to call after filling]
    variable StackFrameHeadersBeingFilled 

    # stores a location of debugger window (for current session only)
    variable LispDebuggerGeometry 

    catch {font create tkconfixed -family Courier -size -20}

    proc StepperMenuTitle {} {
        return "Ходьба"
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
        set RowPath [split $RowName _]
        set WeBelieveThatFramePart [lindex $RowPath 0]
        dict for {frameNo item} $StackFrameHeaders {
            if {[dict get $item RowName] eq $WeBelieveThatFramePart} {
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
                    error "Ура! Меня никто не написал!"
                }
                default {
                    break
                }
            }
        }
        error "неверный RowName = $RowName"
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
                error "Я думал, что кадр $lframe должен быть консом"
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
        set contents "\[Ловушка (catch tag)\] $CatchTag"
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
                    error "Что-то не так с отладчиком: ошибка при отображении локальных переменных"
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
        set TblForLisp [::tkcon::QuoteTclStringForLisp $tbl]
        ::tkcon::EvalInSwankAsync                                                   \
            "(clcon-server:ldbg-edit-frame-source-location $FrameNo $TblForLisp)"   \
            {} [GetDebuggerThreadId]
    }

    proc EditInterpretedFrameSource {tbl RowName} {
        set FrameNo [RowNameToFrameNo $RowName]
        set TblForLisp [::tkcon::QuoteLispObjToString $tbl]
        ::tkcon::EvalInSwankAsync                                                   \
            "(clcon-server::ldbg-edit-interpreted-frame-source-location $FrameNo $TblForLisp)"   \
            {} [GetDebuggerThreadId]
    }


    proc НайтиИсходникИнтерпретируемогоКодаИлиЗначенияЛокальнойПеременной {tbl RowName} {
        set ItemInfo [RowToItemInfo $tbl $RowName]
        set type [lindex $ItemInfo 0]
        switch -exact $type {
            "Local" {
                НайтиИсходникЗначенияЛокальнойПеременной $tbl $RowName
            }
            "Frame" {
                EditInterpretedFrameSource $tbl $RowName
            }
        } 
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

    # Just as if user typed asdf::e in topmost frame. See also EditCurrentAsdfSystem
    proc EditCurrentAsdfFile {} {
        set thread [GetDebuggerThreadId]
        ::tkcon::EvalInSwankAsync \
            "(swank::pprint-eval-string-in-frame \"asdf::e\" 0 \"ASDF\")" \
            {::ldbg::EvalInFrameC2 $EventAsList} $thread
    }

    # Just as if user typed asdf::ep in topmost frame. See also EditCurrentAsdfFile
    proc EditCurrentAsdfSystem {} {
        set thread [GetDebuggerThreadId]
        ::tkcon::EvalInSwankAsync \
            "(swank::pprint-eval-string-in-frame \"asdf::ep\" 0 \"ASDF\")" \
            {::ldbg::EvalInFrameC2 $EventAsList} $thread
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
            НайтиИсходникИнтерпретируемогоКодаИлиЗначенияЛокальнойПеременной { 
                НайтиИсходникИнтерпретируемогоКодаИлиЗначенияЛокальнойПеременной $tbl $RowName
            }
            ReturnFromFrame {
                ReturnFromFrame $RowName
            }
            RestartFrame {
                RestartFrame $RowName
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
            НапечататьСтекВКонсоли {
                # Решение уровня "тяп-ляп". Нужно написать спец.функцию для этого
                EvalInFrameGivenExpressionPrettyPrint $RowName "(let ((*print-circle* nil) (*print-length* (or *print-length* 502)) (*print-level* (or *print-level* 502))) (progn (print (swank::debugger-info-for-emacs 0 499) *trace-output*) :Смотри-стек-в-консоли-сервера))" 
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
            error "Я ждал конс: $TitleListL"
        }
        set TitleList [::mprs::Unleash $TitleListL]
        set e1 [::mprs::Unleash [lindex $TitleList 0]]
        set e2 [::mprs::Unleash [lindex $TitleList 1]]
        ::tkcon::WritePassiveText $w "$e1\n" end
        ::tkcon::WriteActiveText $w $e2 end "::ldbg::InspectCurrentCondition" error
    }
    
    proc ExtractStackFrames { EventAsList } {
        set frames [lindex $EventAsList 5]
    }

    proc EnableDisableMenus { DbgToplevelWindow } {
        #variable StepperMode
        #variable StepperMenuPathname
        #if {$StepperMode} {
        #    set state normal
        #} else {
        #    set state disabled
        #}
        # showVar StepperMenuPathname
        
        # set DebuggerMenuBar [string cat $DbgToplevelWindow .mbar]
        # $DebuggerMenuBar entryconfigure [StepperMenuTitle] -state $state
    }
    
    # Точка входа в отладчик - продолжение назначенное на ответ на запрос об инициализации
    proc StartDebugging { EventAsList } {
        variable DebugEvent
        variable Restarts
        variable StepperMode
        variable LispDebuggerGeometry
        variable InTheDebugger

        set DebugEvent $EventAsList
        # showVar EventAsList

        set rasm [::ldbg::ParseRestarts]
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
        ::win_lay::PositionATool $w
        DoGoToTop $w
        
        # if {[info exists LispDebuggerGeometry]} {
        #     showVar LispDebuggerGeometry
        #     wm geometry $w $LispDebuggerGeometry
        # }

        if {$StepperMode} {
            # In a stepper mode, expand locals at THIRD stack frame
            $tbl expand 0
            # In a stepper mode, show source immediately
            CellCmd 0 RowDblClick
        } else {
            focus [$tbl bodypath]
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

    proc MakeMainWindowFileMenu {w menu} {
        set m [menu [::tkcon::MenuButton $menu "1.Файл" файл]]
        set cmd "::ldbg::EditCurrentAsdfFile" 
        $m add command -label "1. Редактировать текущий файл ASDF, если он определён" -underline 0 -command $cmd

        set cmd "::ldbg::EditCurrentAsdfSystem" 
        $m add command -label "2. Редактировать текущую систему ASDF, если она определена" -underline 0 -command $cmd

        set cmd "::ldbg::ThrowToTopLevel $w"
        $m add command -label "3. Закрыть отладчик и вызвать перезапуск по умолчанию" -underline 0 -command $cmd
    }
        

    proc MakeMainWindowStackMenu {w menu} {
        set m [menu [::tkcon::MenuButton $menu "2.Стек" stack]]
        set tbl [GetFramesTablelist $w ]

        # FIXME redo all that strings to lists as with EnableStepping
        set cmd "::ldbg::CellCmdForActiveCell $tbl RowDblClick"
        $m add command -label "Перейти к определению/инспектор локальной переменной" -accel "Return" -command $cmd
        #
        set cmd "::ldbg::CellCmdForActiveCell $tbl НайтиИсходникИнтерпретируемогоКодаИлиЗначенияЛокальнойПеременной"
        $m add command -label "0.Найти исходник интерпретируемого кода или значения локальной переменной" -underline 0 -command $cmd

        set cmd "::ldbg::InspectCurrentCondition"
        $m add command -label "1.Смотреть исключение в инспекторе" -underline 0 -command $cmd

        set cmd "::ldbg::CellCmdForActiveCell $tbl ReturnFromFrame"
        $m add command -label "2.Вернуться из кадра..." -underline 0 -command $cmd

        set cmd "::ldbg::CellCmdForActiveCell $tbl RestartFrame"
        $m add command -label "3.Перезапустить кадр" -underline 0 -command $cmd

        set cmd "::ldbg::CellCmdForActiveCell $tbl EvalInFrame"
        $m add command -label "4.Выполнить в кадре..." -underline 0 -command $cmd

        set cmd "::ldbg::CellCmdForActiveCell $tbl EvalInFramePrettyPrint"
        $m add command -label "5.Выполнить в кадре и лепо вывести..." -underline 0 -command $cmd

        set cmd "::ldbg::CellCmdForActiveCell $tbl НапечататьСтекВКонсоли"
        $m add command -label "6.Напечатать первые 500 кадров стека в консоли" -underline 0 -command $cmd

        set cmd "::ldbg::GetNext30"
        $m add command -label "7.Получить ещё 30 кадров стека" -underline 0 -command $cmd
    }

    proc GetNext30 {} {
        variable FramesRead
        if { $FramesRead > 0 } {
            set thread [GetDebuggerThreadId]
            set Last [expr $FramesRead + 29]
            ::tkcon::EvalInSwankAsync                           \
                "(swank::debugger-info-for-emacs $FramesRead $Last)" \
                "::ldbg::GetNext30Cont \$EventAsList"           \
                $thread
        }
    }

    proc GetNext30Cont {EventAsList} {
#        tk_messageBox -message [::mprs::ParseReturnOk $EventAsList]
        variable FramesRead
        set n [expr $FramesRead + 30]
        set FramesRead 0 
        if { [lindex $EventAsList 0] eq {:return} } {
            set SwankReply [::mprs::Unleash [lindex $EventAsList 1]]
            set HeadSwankReply [lindex $SwankReply 0]
            if {$HeadSwankReply eq {:ok}} {
                set frames [lindex [::mprs::Unleash [lindex $SwankReply 1]] 2]
                InsertSeveralFramesIntoTree $frames
                if { [llength $frames] == 31 } {
                    set FramesRead $n
                }
            }
        }
    }
    
    proc MakeMainWindowEditMenu {w menu} {
        set m [menu [::tkcon::MenuButton $menu "3.Правка" edit]]
        set tbl [GetFramesTablelist $w ]
        set bodytag [$tbl bodytag]

        set cmd [list ::tk_textCopy $w.title.text]
	$m add command -label "1.Копировать выделенную часть описания ошибки" -underline 0 \
            -command $cmd

        set cmd "::tablelist_util::CopyCurrentCell $tbl"
	$m add command -label "Копировать тек.ячейку"  -accel "Control-C" \
            -command $cmd
        
        ::clcon_key::b bind $bodytag <Control-Key-c> $cmd
        bind $bodytag <Control-Key-Insert> $cmd

        set cmd [list ::fndrpl::OpenFindBox $tbl "tablelist" "find" "::ldbg::FramesTablelistEnsurePopulated"]
	$m add command -label "Искать"  -accel "Control-f" -command $cmd
        ::clcon_key::b bind $bodytag <Control-Key-f>	   $cmd
     

        set cmd [list ::fndrpl::FindIt $tbl]
	$m add command -label "Найти далее" -underline 0 -command $cmd -state disabled
        #bind $tbl <F3> $cmd
        
        $m add separator

        ::tkcon::ВставитьВМенюПунктыПроШрифты $m $w {{Виджет КодРазмера} {
            variable ::tkcon::OPT
            set Шрифт [lindex $::tkcon::OPT(шрифты) ${КодРазмера}]
            set w [::ldbg::GetFramesTablelist ${Виджет}]
            $w configure -font ${Шрифт}
            set w [::ldbg::GetTitleTextWidget ${Виджет}]
            $w configure -font ${Шрифт}
        }}        
        # set cmd ::srchtblst::
    }
    
    proc MakeMainWindowWindowMenu {w menu} {
        variable ::tkcon::COLOR
        set m [::tkcon::MenuButton $menu "7.Окно" окно]
	menu $m -disabledforeground $COLOR(disabled) \
		-postcommand [list ::window_menu::DynamicWindowMenu $w $m]
        ::window_menu::WindowMenuKeyBindings $w $w $w
    }

    # Fills Restarts variable
    proc ParseRestarts {} {
        variable DebugEvent
        set RestartsL [lindex $DebugEvent 4]
        if {[::mprs::Null $RestartsL]} {
            return [list "" 0]
        }
        if {![::mprs::Consp $RestartsL]} {
            error "Ожидался конс: $RestartsL"
        }
        set restarts [::mprs::Unleash $RestartsL]
        set stepperMode 0

        foreach r $restarts {
            set restartName [::mprs::Unleash [::mprs::Car $r]]
            # есть также ф-я SWANK/BACKEND:SLDB-STEPPER-CONDITION-P в swank,
            # к-рая делает примерно то же самое
            switch -nocase $restartName {
                step-into {
                    set stepperMode 1
                    break
                }
                step-next {
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
        puts "; Выход на уровень $level"
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
            error "LocalInspectValue: это не локальная переменная"
        }
        set LocalNo [lindex $ItemInfo 1]
        set LocalItem [lindex $ItemInfo 2]
        set FrameItem [lindex $ItemInfo 3]
        set FrameNo [dict get $FrameItem "FrameNo"]
        set thread [GetDebuggerThreadId]
        #
        ::tkcon::EvalInSwankAsync                           \
            "(swank:inspect-frame-var $FrameNo $LocalNo)"   \
            "::insp::SwankInspect1 \$EventAsList $thread"           \
            $thread
    }

    # Ищет исходник выбранной переменной в clco:*MY-LOCATIONS-HASH* и открывает его
    # Args: RowName of variable entry
    proc НайтиИсходникЗначенияЛокальнойПеременной {tbl RowName} {
        set ItemInfo [RowToItemInfo $tbl $RowName]
        set type [lindex $ItemInfo 0]
        if {$type ne "Local"} {
            error "НайтиИсходникЗначенияЛокальнойПеременной: выбранный элемент стека - не локальная переменная"
        }
        set LocalNo [lindex $ItemInfo 1]
        set LocalItem [lindex $ItemInfo 2]
        set FrameItem [lindex $ItemInfo 3]
        set FrameNo [dict get $FrameItem "FrameNo"]
        set thread [GetDebuggerThreadId]
        set TblForLisp [::tkcon::QuoteLispObjToString $tbl]
        ::tkcon::EvalInSwankAsync                                                   \
            "(clcon-server::ldbg-edit-local-var-source-location $FrameNo $LocalNo $TblForLisp)"   \
            {} [GetDebuggerThreadId]
    }


    proc InspectCurrentCondition {} {
        set thread [GetDebuggerThreadId]                    
        ::tkcon::EvalInSwankAsync                           \
            " (swank:inspect-current-condition)"            \
            "::insp::SwankInspect1 \$EventAsList $thread"           \
            $thread
    }


    proc EnableStepping {} {
        set thread [GetDebuggerThreadId]                    
        ::tkcon::EvalInSwankAsync                           \
            " (cl:if (cl:find-restart 'cl-user::step-into) (swank::sldb-step-into) (cl:progn (swank/backend:activate-stepping nil) (cl:invoke-restart 'cl:continue)))"          \
            {::ldbg::EnableSteppingC1 $EventAsList}      \
            $thread
    }

    proc EnableSteppingC1 {EventAsList} {
       # FIXME а может быть, надо обработать EventAsList? 
       variable MainWindow
       ::ldbg::CloseDebuggerWindow $MainWindow
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

# Example of successful return from frame:
# (:emacs-rex
#  (swank:sldb-return-from-frame 8 "nil")
#  "COMMON-LISP-USER" 10 23)
# (:return
#  (:abort "nil")
#  23)
# (:debug-return 10 1 nil)

# Example of unsuccessful one:
#    (:emacs-rex
# (swank:sldb-return-from-frame 0 "3")
# "COMMON-LISP-USER" 1 8)
#(:return
# (:ok "(\"Cannot return from frame: #<sb-di::compiled-frame sb-kernel::integer-/-integer, interrupted>\")")
# 8)

    proc ReturnFromFrame {RowName} {
        variable MainWindow
        set FrameNo [RowNameToFrameNo $RowName]
        #set level [GetDebuggerLevel]
        set thread [GetDebuggerThreadId]
        foreach {isok code}                                 \
            [ LameAskForLispExpression $MainWindow          \
                  "Введите выражение Лиспа (shift-enter=новая строка) и нажмите Enter: cl-user>"
             ] break
        if {$isok ne "ok"} {
            return
        }
        set qCode [::tkcon::QuoteLispObjToString $code]
        ::tkcon::EvalInSwankAsync \
            "(swank:sldb-return-from-frame $FrameNo $qCode)" \
            {::ldbg::ReturnFromFrameC1 $EventAsList} $thread
    }

    proc ReturnFromFrameC1 {EventAsList} {
        variable MainWindow
        set EventHead [lindex $EventAsList 0]
        if { $EventHead ne {:return} } {
            puts stderr "Что-то не так: в ответе текстового лисп-сервера мы ожидали :return, но получили $EventHead"
        }
        set SwankReply [::mprs::Unleash [lindex $EventAsList 1]]
        set HeadSwankReply [lindex $SwankReply 0]
        switch -exact $HeadSwankReply {
            ":ok" { 
                set message [::mprs::Unleash [lindex $SwankReply 1]]
                puts stderr $message
                ::tkcon::FocusConsole
            }
            ":abort" {
                ::ldbg::CloseDebuggerWindow $MainWindow
            }
        }
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
        EvalInFrameInner $RowName 0 {}
    }

    proc EvalInFramePrettyPrint {RowName} {
        EvalInFrameInner $RowName 1 {}
    }

    proc EvalInFrameGivenExpressionPrettyPrint {RowName StringToQuoteForLisp} {
        EvalInFrameInner $RowName 2 $StringToQuoteForLisp
    }

    # 0 - запросить выражение, вычислить, напечатать (пакет из текущего кадра, StringToQuoteForLisp игнорируется)
    # 1 - запросить, вычислить, лепо напечатать (то же)
    # 2 - вычислить без запроса выражение StringToQuoteForLisp
    proc EvalInFrameInner {RowName Mode StringToQuoteForLisp} {
        set FrameNo [RowNameToFrameNo $RowName]
        ::tkcon::EvalInSwankAsync                                 \
            "(swank:frame-package-name $FrameNo)"                 \
            [subst -nocommand {
                ::ldbg::EvalInFrameC1 $FrameNo $Mode [list $StringToQuoteForLisp] \$EventAsList
            }] [GetDebuggerThreadId]
    }

    proc EvalInFrameC1 {FrameNo Mode StringToQuoteForLisp EventAsList} {
        variable MainWindow
        if { [lindex $EventAsList 0] eq {:return} } {
            if {[lindex [::mprs::Unleash [lindex $EventAsList 1]] 0] eq {:abort}} {
                return
            }
        }
        
        set package [::mprs::ParseReturnOk $EventAsList]
        set qPackage [::tkcon::QuoteLispObjToString $package]

        #set level [GetDebuggerLevel]
        
        if {$Mode != 2} {
            foreach {isok code}                                      \
                [LameAskForLispExpression $MainWindow                \
                     "Выполнить в кадре (shift-enter=новая строка)$package>" \
                    ] break
            if {$isok ne "ok"} {
                return
            }
        } else {
           set code $StringToQuoteForLisp
        }
        puts ";;Выполняю в кадре $FrameNo:"
        puts ";;$package> $code"
        set qCode [::tkcon::QuoteLispObjToString $code]
        set thread [GetDebuggerThreadId]
        if {$Mode != 0} {
            set LispFn "swank:pprint-eval-string-in-frame"
        } else {
            set LispFn "swank:eval-string-in-frame"
        }
        ::tkcon::EvalInSwankAsync \
            "($LispFn $qCode $FrameNo $qPackage)" \
            {::ldbg::EvalInFrameC2 $EventAsList} $thread
    }
        

    proc EvalInFrameC2 {EventAsList} {
        if { [lindex $EventAsList 0] eq {:return} } {
            if {[lindex [::mprs::Unleash [lindex $EventAsList 1]] 0] eq {:abort}} {
                return
            }
        }
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

    proc RestartFrame {RowName} {
        error "ПРАВЬМЯ: Перезапуск кадра не реализован должным образом"
        variable MainWindow
        set FrameNo [RowNameToFrameNo $RowName]
        set thread [GetDebuggerThreadId]
        ::tkcon::EvalInSwankAsync \
            "(swank:restart-frame $FrameNo)" \
            {} $thread
        ::ldbg::CloseDebuggerWindow $MainWindow
    }


    proc InvokeStepperRestart {names ItIsContinue} {
        variable StepperMode
        variable InTheDebugger
        variable MainWindow
        if {!$InTheDebugger} {
            tk_messageBox -parent $MainWindow -title "Ходьба" -message "Отладчик не активен"
        } elseif {!$StepperMode && !$ItIsContinue} {
            tk_messageBox -parent $MainWindow -title "Ходьба" -message "Режим ходьбы не активен"
        } else {
            ::ldbg::ВызватьОдинИзРестартовПоИмени $names
        }
    }
    
    # Name must be a readable qualified lisp symbol, e.g. cl-user::step-out
    proc ВызватьОдинИзРестартовПоИмени {names} {
        set thread [GetDebuggerThreadId]
        # showVar names
        set LispCmd "(clco::restart-with-name-exists-p '$names)"
        # showVar LispCmd
         
        ::tkcon::EvalInSwankAsync $LispCmd \
            [subst -nocommands {
                ::ldbg::ВызватьОдинИзРестартовПоИмениПр1 \$EventAsList [list $names]
            }] $thread
    }


    proc ВызватьОдинИзРестартовПоИмениПр1 {EventAsList names} {
        variable MainWindow
        set thread [GetDebuggerThreadId]
        set name [::mprs::ParseReturnOk $EventAsList]
        if {!($name eq 0)} {
            # передача символа выглядит довольно гротескно... 
            set Пакет [::mprs::Unleash [lindex $name 0]]
            set Имя [::mprs::Unleash [lindex $name 1]]
            set цПакет [::tkcon::QuoteLispObjToString ${Пакет}]
            set цИмя [::tkcon::QuoteLispObjToString ${Имя}]
            set LispCmd "(cl:invoke-restart (cl:find-symbol ${цИмя} ${цПакет}))"
            ::tkcon::EvalInSwankAsync $LispCmd {} $thread
        } else {
            tk_messageBox -parent $MainWindow -title "Restart" -message "Команда(ы) перезапуска $names не найдена(ы)"
        }
        # Note we close the window just now, not after idle, as this can be stepper restart
        ::ldbg::CloseDebuggerWindow $MainWindow
    }

    # Currently unused. Has counterpart in slime. Seem to be useless.
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
    


    proc ThrowToTopLevel {w} {
        variable DebugEvent
        variable Restarts
        variable MainWindow
        set thread [GetDebuggerThreadId]
        set level [GetDebuggerLevel]
        ::tkcon::EvalInSwankAsync \
            "(swank:throw-to-toplevel)" \
            {} $thread
        ::ldbg::CloseDebuggerWindow $MainWindow
    }
    

    
    ## ::tkcon::HistoryMenu - dynamically build the menu for attached interpreters
    ##
    # ARGS:	m	- menu widget
    ##
    proc FillRestartsMenu {w m} {
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
                set underlined "-underline 0"
            } else {
                set underlined ""
            }
            set accel [list "" ""]
            if { [string tolower $Short] eq "continue" } {
               set accel [list <F5> <Key-F5>]
            }
            set cmd "::ldbg::InvokeRestart $i" 
            $m add command                     \
                -label "$i: \[$Short\] $Long"  \
                -command $cmd \
                -accel [lindex $accel 0] \
                {*}$underlined
            set accelKey [lindex $accel 1]
            if { {} ne $accelKey } {
                foreach bindtag [DebuggerBindTags $w] {
                    bind $bindtag $accelKey $cmd
                }
            }
            incr i
        }
    }
    
    proc MakeMainWindowRestartsMenu {w menu} {
        variable ::tkcon::COLOR
        set m [::tkcon::MenuButton $menu "4.Команды перезапуска" restarts]
        menu $m -disabledforeground $::tkcon::COLOR(disabled) \
            -postcommand [list ::ldbg::FillRestartsMenu $w $m]
    }

    # For menu m, adds stepper command with label starting named restart (or first existing from list of restarts) and
    # binds it to each of bindtags
    proc StepperMenuItem {m bindtags names label accel commandOverride IsItContinue} {
        variable MainWindow
        if {$commandOverride eq {}} {
          set cmd [list ::ldbg::InvokeStepperRestart $names $IsItContinue]
        } else {
          set cmd $commandOverride
        }
        set cmdBreak "$cmd; break"
        $m add command -label $label -command $cmd -accel $accel
        foreach bt $bindtags {
            bind $bt $accel $cmdBreak
        }
    }

    # m is a menu. It can be in other window!
    # client = {Отладчик} или {Редактор}
    proc FillStepperMenu {m bindtags client} {
        variable StepperMode
        StepperMenuItem $m $bindtags "(cl-user::step-continue cl:continue)" "Беги" <F5> "" 1
        
        if {$StepperMode} {
          StepperMenuItem $m $bindtags "cl-user::step-out" "Выбеги" <Shift-F11> "" 0
          StepperMenuItem $m $bindtags "cl-user::step-next" "Переступи" <F10> "" 0
        }
        StepperMenuItem $m $bindtags "cl-user::step-into" "Зайди" <F11> "::ldbg::EnableStepping" 0
    }
    
    proc DebuggerBindTags {w} {
        set tbl [GetFramesTablelist $w]
        set bodytag [$tbl bodytag]
        list $w $tbl [GetTitleTextWidget $w] $bodytag
    }

    proc MakeMainWindowStepperMenu {w menu} {
        variable StepperMenuPathname
        set m [menu [::tkcon::MenuButton $menu [StepperMenuTitle] stepper]]
        set StepperMenuPathname $m
        FillStepperMenu $m [DebuggerBindTags $w] {Отладчик}
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
        set w [::ide_structure::DebuggerToplevelWindowName]
        catch {destroy $w}

        #if {[winfo exists $w]} {
        #    ClearStackFramesTableList
        #    return $w
        #}

        set metrics [font measure [ tkcon font ] "w"]
        toplevel $w -width [expr { 50 * $metrics }]
        wm withdraw $w

     
        # title
        set level [GetDebuggerLevel]
        set word "Отладчик, уровень $level"
        wm title $w $word

        set MainWindow $w

        # --------------- frames, tablelist -----------------
        # --------------- title (condition description) ------
        frame $w.title
        ::clcon_text::clcon_text $w.title.text -height 12 -readonly 1
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
            -columns {60 "Кадры стека" left} \
            -stretch 0 -spacing 10 \
            -width 62   \
            -exportselection 0 \
            -expandcommand "::ldbg::FramesTablelistEnsurePopulated"
        
        $tbl resetsortinfo 

        $tbl configure \
            -foreground \#000000 \
            -font [ ::tkcon font ] -borderwidth 1 -highlightthickness 1 
        

        $tbl columnconfigure 0 -wrap true  

        # ----------------------------------- menu bar -------------------
        
        set menu [menu $w.mbar]
        $w configure -menu $menu
        # Menu items created later as they refer to window contents
        
        # TitleListFileMenu $w $menu
        MakeMainWindowFileMenu $w $menu
        MakeMainWindowStackMenu $w $menu
        MakeMainWindowEditMenu $w $menu
        MakeMainWindowWindowMenu $w $menu
        MakeMainWindowRestartsMenu $w $menu

        # StepperMode is set in the caller fn.
        MakeMainWindowStepperMenu $w $menu

        # ------------------------------ bindings -------------
        MakeBindings $w
        wm protocol $w WM_DELETE_WINDOW "::ldbg::ThrowToTopLevel $w"
        
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
