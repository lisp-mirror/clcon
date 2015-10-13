namespace eval ::edt {
    proc OduFnMenuItem {w m btext oduCmd {accel {}} {bindtag {}}} {
        set oduFn [string cat "odu::" $oduCmd "-command"]
        set cmd [wesppt [list clcon_text::CallOduvanchikFunction $btext "$oduFn nil"]]
        $m add command -label $oduCmd -accel $accel -command $cmd
        if {$accel ne {}} {
            bind $bindtag $accel "$cmd; break"
        }
        return $cmd
    }

    proc MakeLispModeMenu {w btext} {

        set m [cMenuBar .lisp]
        ClearMenu $m
        
        # It is too late hour to start show-mark
        # We have archietectural problems there (rompsite.lisp is too early on the build)
        # set oduCmd "lisp-insert-\)"
        # set cmd [wesppt [list clcon_text::CallOduvanchikFunction $btext ????]]
        # $m add command -label $oduCmd -accel "F11" -command $cmd
        # bind $w <F11> $cmd

        set cmd [list ::edt::CompileAndLoadTheFile $btext]
        $m add command -label "0.Compile and load" -underline 0 -command $cmd -accel "F5"
        bind NoMod$w <F5> $cmd

        ::erbr::AddNextAndPreviousCompilerMessagesCommands $m $btext 1

        $m add separator

        set cmd [OduFnMenuItem $w $m $btext indent-new-line "<Shift-Return>" SingleMod$w]
        # bind SingleMod$w <Shift-Return> "$cmd; break"

        OduFnMenuItem $w $m $btext indent-form
        
        set cmd [wesppt [list clcon_text::CallOduvanchikFunction $btext "odu::indent-region-command nil" {} {send_selection 1}]]
        
        $m add command -label "Indent Region" -accel "F11" -command $cmd 
        bind NoMod$w <F11> $cmd
        
        
        OduFnMenuItem $w $m $btext transpose-forms
        $m add separator
        OduFnMenuItem $w $m $btext beginning-of-defun
        OduFnMenuItem $w $m $btext end-of-defun
        OduFnMenuItem $w $m $btext mark-defun 
        $m add separator

        OduFnMenuItem $w $m $btext forward-form <Control-Key-Right> SingleMod$w
        
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

    # Empties menu (except menu bar) and fills it again
    proc RebuildMenu {} {
        # set menu [cMenuBar]
        
        ## File Menu
        ## Note that this is not a menu creation command!
        set w [cW]
        set btext [c_btext]
        
        set m [cMenuBar .file]
        ClearMenu $m

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

        set CloseFile [wesppt [list ::edt::EditCloseCurrentFile]]
        $m add command -label "Close" -accel "Control-w" -command $CloseFile
        bind SingleMod$w <Control-Key-w> $CloseFile
        
        set dismiss [wesppt [list ::edt::HideEditorWindow [cTW]]]
        $m add command -label "Hide editor window" -underline 0 -command $dismiss
        $m add command -label "4.Reload some of IDE sources" -underline 0 \
	    -command ::tkcon::ReloadSomeIDESources

        
        ## Edit Menu
        ##
        set m [cMenuBar .edit]
        ClearMenu $m

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

        ## Lisp mode Menu
        ##
        MakeLispModeMenu $w $btext
        
        ## Tcl mode Menu
        ## 
        set m [cMenuBar .tcl]
        ClearMenu $m
        
        set SendToSlave [wesppt "::tkcon::EvalSlave \
		    eval \[$btext get 1.0 end-1c\]"]
        $m add command -label "&2. Send Text To Slave" \
            -command $SendToSlave

        $m add separator

        set cmd "::edt::e_indent $btext"
        $m add command -label "Tcl indent new line" -accel <Control-Key-Return> -command $cmd
        bind DoubleMod$w <Control-Key-Return> "$cmd; break"
        bind NoMod$w <F4> "puts WOW; $cmd; break"

        
        ## Window Menu
        ##
        set m [cMenuBar .window]
        ClearMenu $m
        
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
        ClearMenu $m
        
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