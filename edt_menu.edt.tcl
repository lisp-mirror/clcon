namespace eval ::edt {

    # This is a primitive for commands which behave differently. If there is oduvan backend,
    # script is called at uplevel 1. Otherwise, continue is called at uplevel 1
    proc CodeToCallBackendOrContinue {btext script_when_backend} {
        set result [subst -nocommands {if {[$btext UsesLispP]} {
  $script_when_backend
 } else {
  continue
 }}]
        return $result
    }

    proc EditNewFile {} {
        ::edt::edit -type newfile -- ""
    }

    proc CurrentBufferPathnameToClipboard {style} {
        set clcon_text [c_btext]
        set FileNameUnix [[$clcon_text cget -opened_file] cget -filename]
        switch -exact -- $style {
            unix {
                set FileName $FileNameUnix
            }
            windows {
                set FileName [::UnixFileNameToWindows $FileNameUnix]
            }
            default {
                error "Неожиданный стиль $style"
            }
        }
        clipboard clear
        clipboard append $FileName
    }

    # Types in buffer pathname to console. See also ::tkcon::PasteAsLinuxFilename
    proc CurrentPathAndFileNameToConsole {ignore} {
        variable ::tkcon::PRIV
        set con $PRIV(console)
        set clcon_text [c_btext]
        set FileNameUnix [[$clcon_text cget -opened_file] cget -filename]
        set dir [file dirname $FileNameUnix]
        set len [string length $dir]
        set just_file [string range $FileNameUnix $len end]
        set start_index [string cat "insert-" $len "c"] 
        set MaybeDirTypedIn [$con get $start_index insert]
        if {$MaybeDirTypedIn eq $dir} {
            $con insert insert $just_file
        } else {
            $con insert insert $dir
        }
    }

    # Extracts selection text. To use in utilities and user-level functions
    proc GetTextSelectedInCurrentEditor {} {
        set w [::edt::c_btext]
        if {[$w tag nextrange sel 1.0 end] != ""} {
            return [$w get sel.first sel.last]
        } else {
            return ""
        }
    }
        
    # If ContinueIfNoBackend, binding would check precense of oduvan-backend,
    # and if it is absent, would call continue so that other bindings would work.
    # Returns cmd with break. 
    proc OduFnMenuItem {w m btext oduCmd args} {
        named_args $args {-accel {} -bindtag {} -ContinueIfNoBackend 0 -CallOduvanchikFunctionOptions {}} 
        set oduFn [string cat "odu::" $oduCmd "-command"]
        set ScriptForBackend \
            [wesppt [list clcon_text::CallOduvanchikFunction $btext \
                          "$oduFn nil" "" $(-CallOduvanchikFunctionOptions)]] 
        set ScriptForBackendBreak \
            [wesppt [list clcon_text::CallOduvanchikFunction $btext \
                          "$oduFn nil" "" $(-CallOduvanchikFunctionOptions)] \
                    -add-break 1] 
        if {$(-ContinueIfNoBackend)} {
            set cmd [CodeToCallBackendOrContinue $btext $ScriptForBackend]
            set cmdBreak [CodeToCallBackendOrContinue $btext $ScriptForBackendBreak]
        } else {
            set cmd $ScriptForBackend
            set cmdBreak $ScriptForBackendBreak
        }
        $m add command -label $oduCmd -accel $(-accel) -command $cmd
        if {$(-accel) ne {}} {
            bind $(-bindtag) $(-accel) $cmdBreak
        }
        return $cmdBreak
    }

    proc MakeLispModeMenu {Bi w btext} {

        set m [cMenuBar .lisp]
        ::gui_util::ClearMenu $m
        
        # It is too late hour to start show-mark
        # We have archietectural problems there (rompsite.lisp is too early on the build)
        # set oduCmd "lisp-insert-\)"
        # set cmd [wesppt [list ::clcon_text::CallOduvanchikFunction $btext ????]]
        # $m add command -label $oduCmd -accel "F11" -command $cmd
        # bind $w <F11> $cmd

        ::erbr::AddNextAndPreviousCompilerMessagesCommands $m SingleMod$w 1

        $m add separator

        # Stepper menu
        set StepperMenu $m.stepper
        catch { destroy $StepperMenu }
        $m add cascade -label [::ldbg::StepperMenuTitle] -state disabled -menu $StepperMenu
        menu $StepperMenu
        # This is not very correct as we have some stepper commands with single mod.
        # But we avoid complification of things this way
        # FIXME if we use special bindtags and reconfigure tags when stepper enabled/disabled,
        # we can reuse that keys.
        ::ldbg::FillStepperMenu $StepperMenu [list NoMod$w] 
        
        $m add separator

        OduFnMenuItem $w $m $btext indent-new-line -accel "<Shift-Key-Return>" -bindtag SingleMod$w

        OduFnMenuItem $w $m $btext indent-form

        OduFnMenuItem $w $m $btext indent 

        OduFnMenuItem $w $m $btext indent-or-complete-symbol-with-budden-tools -accel "<Tab>" -bindtag NoMod$w

        set cmd [wesppt [list clcon_text::CallOduvanchikFunction $btext "odu::indent-region-command nil" {} {send_selection 1}]]
        
        $m add command -label "Проставить отступ в выденном фрагменте" -command $cmd 
       
        OduFnMenuItem $w $m $btext transpose-forms
        $m add separator
        OduFnMenuItem $w $m $btext beginning-of-defun
        OduFnMenuItem $w $m $btext end-of-defun
        OduFnMenuItem $w $m $btext mark-defun 
        $m add separator

        # ------------------------ modified right key ---------------------------
    
        #set cmd [wesppt [list event generate $btext <<NextWord>>] -add-break 1] 
        #::clcon_key::b bind SingleMod$w <Alt-Key-Right> $cmd

        OduFnMenuItem $w $m $btext forward-form-or-word \
            -ContinueIfNoBackend 1              \
            -accel <Control-Key-Right> -bindtag SingleMod$w 

        OduFnMenuItem $w $m $btext forward-form-or-word-altering-selection \
            -ContinueIfNoBackend 1                                 \
            -accel <Control-Shift-Key-Right> -bindtag DoubleMod$w  \
            -CallOduvanchikFunctionOptions {send_selection 1}

        OduFnMenuItem $w $m $btext forward-character-altering-selection \
            -ContinueIfNoBackend 1                                 \
            -accel <Shift-Key-Right> -bindtag SingleMod$w  \
            -CallOduvanchikFunctionOptions {send_selection 1}
        
        # ------------------------ modified left key ---------------------------

        #set cmd [wesppt [list event generate $btext <<PrevWord>>] -add-break 1] 
        #::clcon_key::b bind SingleMod$w <Alt-Key-Left> $cmd

        OduFnMenuItem $w $m $btext backward-form-or-word \
            -ContinueIfNoBackend 1              \
            -accel <Control-Key-Left> -bindtag SingleMod$w 

        OduFnMenuItem $w $m $btext backward-form-or-word-altering-selection \
            -ContinueIfNoBackend 1                                 \
            -accel <Control-Shift-Key-Left> -bindtag DoubleMod$w  \
            -CallOduvanchikFunctionOptions {send_selection 1}


        OduFnMenuItem $w $m $btext backward-character-altering-selection \
            -ContinueIfNoBackend 1                                 \
            -accel <Shift-Key-Left> -bindtag SingleMod$w  \
            -CallOduvanchikFunctionOptions {send_selection 1}

        
        OduFnMenuItem $w $m $btext forward-list
        OduFnMenuItem $w $m $btext backward-list
        OduFnMenuItem $w $m $btext forward-up-list \
            -ContinueIfNoBackend 1              \
            -accel <Alt-Key-Up> -bindtag SingleMod$w 

        OduFnMenuItem $w $m $btext backward-up-list
        OduFnMenuItem $w $m $btext down-list

        $m add separator

        set cmd [wesppt [list ::edt::FindSourceCommand $btext]]
        $m add command -label "Перейти к определению" -accel "Alt-." -command $cmd
        ::clcon_key::b bind SingleMod$w <Alt-period> $cmd

        $m add separator
        set cmd [wesppt [list ::edt::LispDescribeAllCommand $btext]]
        $m add command -label "Справка по идентиф-ру" -accel "Key-F1" -command $cmd
        ::clcon_key::b bind SingleMod$w <Key-F1> $cmd

        set cmd [wesppt [list ::edt::FindSymbolCommand $btext]]
        $m add command -label "Скопировать идентф-р в буфер обмена" -accel "Key-F2" -command $cmd
        ::clcon_key::b bind SingleMod$w <Key-F2> $cmd

        set cmd [wesppt [list ::edt::FindPackage $btext]]
        $m add command -label "Перейти к определению пакета" -command $cmd

        set cmd [wesppt [list ::edt::FindSystem $btext]]
        $m add command -label "Перейти к определению системы" -command $cmd

        set cmd [wesppt [list ::edt::CompileSystem $btext]]
        $m add command -label "Скомпилировать и загрузить систему" -command $cmd
    }

    proc EnableDisableMenuItems {} {
        variable ::ldbg::InTheDebugger
        variable ::ldbg::StepperMode
        if {$InTheDebugger && $StepperMode} {
            set st normal
        } else {
            set st disabled
        }
        set stepperMenu [cMenuBar .lisp]
        $stepperMenu entryconfigure [::ldbg::StepperMenuTitle] -state $st
    }
    
    # Empties menu (except menu bar and recent menu) and fills it again
    proc RebuildMenu {} {
        variable ::tkcon::COLOR
        # set menu [cMenuBar]
        
        ## File Menu
        ## Note that this is not a menu creation command!
        set Bi [cBi]
        set w [cW]
        set tw [theTW]
        set btext [c_btext]
        
        set m [cMenuBar .file]
        ::gui_util::ClearMenu $m

        set cmd ::edt::EditNewFile 
        $m add command -label "Создать" -command $cmd

        set initialdir ""
        catch { ::clcon_text::PathToAFile $btext } initialdir
        set cmd [list ::tkcon::OpenForEdit $tw "" $initialdir]
	$m add command -label "Открыть" -command $cmd -accel "Control-Key-O"
        ::clcon_key::b bind SingleMod$w <Control-Key-o> "$cmd; break"

        set cmd [wesppt [list ::edt::Save $Bi $w.text 1]]
        $m add command -label "Сохранить" -command $cmd -accel "Control-S"
        ::clcon_key::b bind SingleMod$w <Control-Key-s> $cmd
        
        $m add command -label "Сохранить как..."  -underline 0 \
            -command [wesppt [list ::edt::SaveAs $Bi $w.text]]
        $m add separator

        set cmd [list ::edt::CompileAndLoadTheFile $Bi]
        $m add command -label "Сохранить, компилировать и загрузить этот файл" -command $cmd -accel "F7"
        bind NoMod$w <F7> $cmd

        $m add separator

        set cmd {::edt::CurrentBufferPathnameToClipboard "unix"}
        $m add command -label "1.Копир.имя файла в буфер обмена (стиль unix)" \
            -underline 0 -command $cmd

        set cmd {::edt::CurrentBufferPathnameToClipboard "windows"}
        $m add command -label "2.Копир.имя файла в буфер обмена (стиль windows)" \
            -underline 0 -command $cmd

        $m add separator

        set cmd [wesppt [list ::edt::EditFileNameUnderCursorCommand $btext]]
        $m add command -label "3.Редактировать файл, имя которого под курсором" -underline 0 -command $cmd
        
        $m add separator    

        $m add command -label "4.Перезагрузить часть исходных текстов ИСР (IDE)" -underline 0 \
	    -command ::tkcon::ReloadSomeIDESources

        ## Recent menu (clone from clcon.tcl, but we need to delete menu as this
        ## code is called many times) 
        set s $m.recent
        if {[winfo exists $s]} {
            destroy $s
        }
        menu $s -disabledforeground $COLOR(disabled) -postcommand [list ::recent::RecentMenu $m]
 	$m add cascade -label "5.Открыть недавний..." -underline 0 -underline 0 -menu $s

        $m add separator
        set CloseFile [wesppt [list ::edt::EditCloseFile $Bi]]
        $m add command -label "Закрыть" -accel "Control-w" -command $CloseFile
        ::clcon_key::b bind SingleMod$w <Control-Key-w> $CloseFile
        

        
        
        ## Edit Menu
        ##
        set m [cMenuBar .edit]
        ::gui_util::ClearMenu $m
        set cmd [wesppt [list tk_textCut $btext]]
        $m add command -label "Вырезать"   -underline 2 \
            -command $cmd
        set cmd [wesppt [list tk_textCopy $btext]]
        $m add command -label "Копировать"  -underline 0 \
            -command $cmd -accel "Control-C"           
        ::clcon_key::b bind SingleMod$w <Control-Key-C> $cmd 
        set cmd [wesppt [list tk_textPaste $btext]]
        $m add command -label "Вставить" -underline 0 \
            -command $cmd -accel "Control-V"
        ::clcon_key::b bind SingleMod$w <Control-Key-igrave> $cmd
        ::clcon_key::b bind SingleMod$w <Control-Key-Igrave> $cmd
           
        ##
        $m add separator
	set cmd [wesppt [list ::fndrpl::OpenFindBox $btext "text" "find" {}]]
        $m add command -label "Искать" -underline 0 -command $cmd -accel "Control-F"
        ::clcon_key::b bind SingleMod$w <Control-Key-f> $cmd

        set cmd [list ::fndrpl::FindIt $btext]
	$m add command -label "Найти далее"  -underline 0 -accel "F3" -command $cmd 
        bind NoMod$w <F3> $cmd

	set cmd [wesppt [list ::fndrpl::OpenFindBox $btext "text" "replace" {}]]
        $m add command -label "Найти и заменить" -under 0 -command $cmd -accel "Control-h"
        ::clcon_key::b bind SingleMod$w <Control-Key-h> "$cmd; break"

        $m add separator
        # Следующие команды работают для всех режимов, поэтому помещаем их в меню "Правка"

        set cmd [list ::tkcon::ReturnPos]
        $m add command -label "перейти назад" -accel <Alt-,> -command $cmd
        ::clcon_key::b bind SingleMod$w <Alt-comma> "$cmd; break"

        set cmd [list ::edt::FindCurrentFileDeclarations $btext]
        $m add command -label "Определения в текущем файле (без сохранения файла!)" \
            -command $cmd -accel "F12"
        bind NoMod$w <F12> [concat $cmd ";" break]

        ## Lisp mode Menu
        ##
        MakeLispModeMenu $Bi $w $btext
        
        ## Tcl mode Menu
        ## 
        set m [cMenuBar .tcl]
        ::gui_util::ClearMenu $m
        
        set SendToSlave [wesppt "puts {Если вы редактируете файл, рекомендуется вместо данной команды использовать команду 'Сохранить, компилировать и загрузить этот файл', чтобы была возможность переходить к определениям процедур\nЗагружаю ваш буфер...}; ::tkcon::EvalSlave namespace eval :: \[$btext get 1.0 end-1c\]; puts Done"]
        $m add command -label "1. Отправить текст в подчинённый интерпретатор" \
            -underline 0 -command $SendToSlave

        $m add separator

        set cmd [list ::edt::e_indent $btext]
        $m add command -label "Tcl: вставить строчку с отступом" -accel <Control-Key-Return> -command $cmd
        bind DoubleMod$w <Control-Key-Return> "$cmd; break"
        
        $m add separator
        set cmd [list ::tkcon::TclFindDefinition [$btext RealText]]
        $m add command -label "Tcl: перейти к определению" -accel <Control-Key-F9> -command $cmd
        bind SingleMod$w <Control-Key-F9> "$cmd; break"

       
        ## Window Menu 

        ## Так же как с меню "недавние", нам нужно удалить данное меню, псокольку 
        ## этот код вызывается много раз
        set m [cMenuBar .window] 
        if {[winfo exists $m]} {
            destroy $m
        }

	menu $m -disabledforeground $COLOR(disabled) \
		-postcommand [list ::window_menu::DynamicWindowMenu $w $m]
        ::window_menu::WindowMenuKeyBindings $w SingleMod$w DoubleMod$w


        ## Secret Menu
        ##
        set m [cMenuBar .secret]
        ::gui_util::ClearMenu $m
        
        set cmd [list $btext Unfreeze]
        $m add command -label "1.Разморозить (если одуванчик завис)" -command $cmd

        set cmd [list $btext ResetBackendBuffer]
        $m add command -label "Разморозить буфер в tcl/tk и пересоздать буфер одуванчика" -command $cmd

        set cmd [list ::tkcon::EvalInSwankAsync "(clco::compare-clcon_text-and-oduvanchik-buffer-contents \"$btext\")" {} t]
        $m add command -label "Проверить совпадение буфера одуванчика и tcl/tk" -accel "F8" -command $cmd
        bind NoMod$w <F8> $cmd

        set cmd [wesppt [list ::edt::SyncCursor $btext]]
        $m add command -label "Синхронизировать курсор одуванчика к tcl/tk" -accel "F9" -command $cmd
        bind NoMod$w <F9> $cmd
    }
}
