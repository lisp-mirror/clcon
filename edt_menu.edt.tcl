namespace eval ::edt {

    # This is a primitive for commands which behave differently. If there is oduvan backend,
    # script is called at uplevel 1. Otherwise, continue is called at uplevel 1
    proc CodeToCallBackendOrContinue {script_when_backend} {
        set result \
       "if {\$::tkcon::OPT(oduvan-backend)} {
  $script_when_backend
 } else {
  continue
 }"
        return $result
    }
        
    # If ContinueIfNoBackend, binding would check precense of oduvan-backend,
    # and if it is absend, would call continue so that other bindings would work.
    # Returns cmd with break. 
    proc OduFnMenuItem {w m btext oduCmd args} {
        named_args $args {-accel {} -bindtag {} -ContinueIfNoBackend 0} 
        set oduFn [string cat "odu::" $oduCmd "-command"]
        set ScriptForBackend \
            [wesppt [list clcon_text::CallOduvanchikFunction $btext "$oduFn nil"]] 
        set ScriptForBackendBreak \
            [wesppt [list clcon_text::CallOduvanchikFunction $btext "$oduFn nil"] \
                 -add-break 1] 
        if {$(-ContinueIfNoBackend)} {
            set cmd [CodeToCallBackendOrContinue $ScriptForBackend]
            set cmdBreak [CodeToCallBackendOrContinue $ScriptForBackendBreak]
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

    proc MakeLispModeMenu {w btext} {

        set m [cMenuBar .lisp]
        ::gui_util::ClearMenu $m
        
        # It is too late hour to start show-mark
        # We have archietectural problems there (rompsite.lisp is too early on the build)
        # set oduCmd "lisp-insert-\)"
        # set cmd [wesppt [list clcon_text::CallOduvanchikFunction $btext ????]]
        # $m add command -label $oduCmd -accel "F11" -command $cmd
        # bind $w <F11> $cmd

        set cmd [list ::edt::CompileAndLoadTheFile $btext]
        $m add command -label "0.Compile and load" -underline 0 -command $cmd -accel "F7"
        bind NoMod$w <F7> $cmd

        ::erbr::AddNextAndPreviousCompilerMessagesCommands $m $btext 1

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

        OduFnMenuItem $w $m $btext indent-new-line -accel "<Shift-Return>" -bindtag SingleMod$w

        OduFnMenuItem $w $m $btext indent-form
        
        set cmd [wesppt [list clcon_text::CallOduvanchikFunction $btext "odu::indent-region-command nil" {} {send_selection 1}]]
        
        $m add command -label "Indent Region" -command $cmd 
       
        OduFnMenuItem $w $m $btext transpose-forms
        $m add separator
        OduFnMenuItem $w $m $btext beginning-of-defun
        OduFnMenuItem $w $m $btext end-of-defun
        OduFnMenuItem $w $m $btext mark-defun 
        $m add separator

        OduFnMenuItem $w $m $btext forward-form \
            -accel <Control-Key-Right> -bindtag SingleMod$w -ContinueIfNoBackend 1
        
        OduFnMenuItem $w $m $btext backward-form
        OduFnMenuItem $w $m $btext forward-list
        OduFnMenuItem $w $m $btext backward-list
        OduFnMenuItem $w $m $btext forward-up-list
        OduFnMenuItem $w $m $btext backward-up-list
        OduFnMenuItem $w $m $btext down-list

        $m add separator

        set cmd [wesppt [list ::edt::FindSourceCommand $btext]]
        $m add command -label "Find Source" -accel "Alt-." -command $cmd
        bind SingleMod$w <Alt-period> $cmd
        bind SingleMod$w <Alt-Key-Cyrillic_yu> $cmd
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
    
    # Empties menu (except menu bar) and fills it again
    proc RebuildMenu {} {
        # set menu [cMenuBar]
        
        ## File Menu
        ## Note that this is not a menu creation command!
        set Bi [cBi]
        set w [cW]
        set btext [c_btext]
        
        set m [cMenuBar .file]
        ::gui_util::ClearMenu $m

        set cmd ::tkcon::OpenForEdit 
	$m add command -label "Open" -command $cmd -accel "Control-O"
        bind SingleMod$w <Control-Key-o> "$cmd; break"
        bind SingleMod$w <Control-Key-Cyrillic_shcha> "$cmd; break"

        set cmd [wesppt [list ::edt::Save $w.text]]
        $m add command -label "Save" -command $cmd -accel "Control-S"
        bind SingleMod$w <Control-Key-s> $cmd
        bind SingleMod$w <Control-Key-Cyrillic_yeru> $cmd
        
        $m add command -label "Save As..."  -underline 0 \
            -command [wesppt [list ::tkcon::Save {} widget $btext]]
        $m add separator

        set CloseFile [wesppt [list ::edt::EditCloseFile $Bi]]
        $m add command -label "Close" -accel "Control-w" -command $CloseFile
        bind SingleMod$w <Control-Key-w> $CloseFile
        
        set dismiss [wesppt [list ::edt::HideEditorWindow [cTW]]]
        $m add command -label "Hide editor window" -underline 0 -command $dismiss
        $m add command -label "4.Reload some of IDE sources" -underline 0 \
	    -command ::tkcon::ReloadSomeIDESources

        
        ## Edit Menu
        ##
        set m [cMenuBar .edit]
        ::gui_util::ClearMenu $m

        $m add command -label "Cut"   -under 2 \
            -command [wesppt [list tk_textCut $btext]]
        $m add command -label "Copy"  -under 0 \
            -command [wesppt [list tk_textCopy $btext]]
        $m add command -label "Paste" -under 0 \
            -command [wesppt [list tk_textPaste $btext]]
        ##
        $m add separator
	set cmd [wesppt [list ::fndrpl::OpenFindBox $btext "text" "find" {}]]
        $m add command -label "Find" -under 0 -command $cmd -accel "Control-F"
        bind SingleMod$w <Control-Key-f> $cmd
        bind SingleMod$w <Control-Key-Cyrillic_a> $cmd

        set cmd [list ::fndrpl::FindIt $btext]
	$m add command -label "Find again"  -underline 0 -accel "F3" -command $cmd 
        bind NoMod$w <F3> $cmd

	set cmd [wesppt [list ::fndrpl::OpenFindBox $btext "text" "replace" {}]]
        $m add command -label "Replace" -under 0 -command $cmd -accel "Control-h"
        bind SingleMod$w <Control-Key-h> "$cmd; break"
        bind SingleMod$w <Control-Key-Cyrillic_er> "$cmd; break"
        
        ## Lisp mode Menu
        ##
        MakeLispModeMenu $w $btext
        
        ## Tcl mode Menu
        ## 
        set m [cMenuBar .tcl]
        ::gui_util::ClearMenu $m
        
        set SendToSlave [wesppt "::tkcon::EvalSlave \
		    eval \[$btext get 1.0 end-1c\]"]
        $m add command -label "1. Send Text To Slave" \
            -underline 0 -command $SendToSlave

        $m add separator

        set cmd "::edt::e_indent $btext"
        $m add command -label "Tcl indent new line" -accel <Control-Key-Return> -command $cmd
        bind DoubleMod$w <Control-Key-Return> "$cmd; break"
        bind NoMod$w <F4> "puts WOW; $cmd; break"

        
        ## Window Menu
        ##
        set m [cMenuBar .window]
        ::gui_util::ClearMenu $m
        
        set cmd [list ::clconcmd::bufferlist]
	$m add command -label "Buffer list" -underline 0 -accel "Control-F12" \
            -command $cmd
        bind SingleMod$w <Control-Key-F12> $cmd
        #
        set cmd [list ::tkcon::FocusConsole]
	$m add command -label "Console" -underline 0 -accel "Control-." \
            -command $cmd
        bind SingleMod$w <Control-Key-period> $cmd
        
        ## Secret Menu
        ##
        set m [cMenuBar .secret]
        ::gui_util::ClearMenu $m
        
        set cmd [list $btext Unfreeze]
        $m add command -label "1.Unfreeze (if oduvanchik hang)" -command $cmd

        set cmd [list ::tkcon::EvalInSwankAsync "(clco::compare-clcon_text-and-oduvanchik-buffer-contents \"$btext\")" {} {:find-existing}]
        $m add command -label "Check Oduvanchik Sync" -accel "F8" -command $cmd
        bind NoMod$w <F8> $cmd

        set cmd [wesppt [list ::edt::SyncCursor $btext]]
        $m add command -label "Sync cursor" -accel "F9" -command $cmd
        bind NoMod$w <F9> $cmd
    }
}
