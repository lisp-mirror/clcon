## edit - opens a file/proc/var for reading/editing
## 
# Arguments:
#   edit ?option value...? -- word
#   word is file/proc/var name
#   options are:
#   -find searchstring
#   -type proc or var or file
#   -wrap wrap option for text widget
#   -offset index to show
#   type	proc/file/var (what about error?)
#   options 
# Returns:	nothing
# Added by budden : -offset option, accepts index
##

namespace eval ::edt {

    proc ShowSomeEditor {} {
        ::tkcon::FocusWindowByName [cTW] [cW]
        return 
    }

    proc HideAllEditorWindows {} {
        puts stderr "REWRITE HideAllEditorWindows"
        variable EditorMRUWinList
        foreach p $EditorMRUWinList {
            set window [Bi2TW [dict get $p "Bi"]]
            if {[winfo exists $window]} {
                wm withdraw $window
            }
        }
    }

    # close file (without saving for now)  
    proc EditCloseCurrentFile {} {
        set Bi [cBi]
        set tw [cTW]
        if {[winfo exists $tw]} {
            wm withdraw $tw
        }
        putd "Saving file if omitted!" 
        RemoveWindowFromLists $Bi
        destroy $tw
        UpdateMRUAndBufferList {}
        ::recent::RedrawRecentMenuForConsole
    }

    # Hides editor window. 
    proc HideEditorWindow {tw} {
        puts stderr "REWRITE HideEditorWindow"
        puts stderr "Normally hiding editor windows assumes closing all files. We are wrong"
        wm withdraw $tw
    }

    # Wrapped for freezed text, for menu only
    proc wesppt {script} {
        ::clcon_text::WrapEventScriptForFreezedText $script [uplevel 1 {string cat "$w.text"}]
    }

    proc LoadContents {w word opts tail} {
        switch -glob -- [dict get $opts -type] {
            proc*	{
                $w.text insert 1.0 \
                    [::tkcon::EvalOther {} slave dump proc [list $word]]
                after idle [list ::tkcon::Highlight $w.text tcl]
            }
            var*	{
                $w.text insert 1.0 \
                    [::tkcon::EvalOther {} slave dump var [list $word]]
                after idle [list ::tkcon::Highlight $w.text tcl]
            }
            file	{

                if {$word ne {}} {
                    set filemtime [file mtime $word]
                } else {
                    set filemtime {}
                }
                
                [$w.text cget -opened_file] configure -filename $word -filemtime $filemtime
                
                ::clcon_text::ConstructBackendBuffer $w.text
                
                $w.text insert 1.0 [::edt::ReadFileIntoString $word 0]
                
                after idle [list ::tkcon::Highlight $w.text \
                                [string trimleft [file extension $word] .]]
            }
            error*	{
                $w.text insert 1.0 [join $tail \n]
                after idle [list ::tkcon::Highlight $w.text error]
            }
            default	{
                $w.text insert 1.0 [join $tail \n]
            }
        }
        $w.text edit reset
        $w.text edit modified 0
        $w.text mark set insert 1.0
    }

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

    
    # Initializes editor GUI, loads text.
    # variable internal_cBi is set already
    # args are for error only
    # Args:
    # tw - editor window pathname, say ".__edit1",
    # w === tw, but in the future it will be editor frame or smth like this
    # opts - editor options
    # tail - editor name argument (filename, procname, errorname, etc.)
    # Important variables:
    #  btext - clcon_text widget (currently is a btext wrapper)
    #  textt - text itself
    # Bindtags:
    #  DoubleKey$w - for double modifiers. Assigned to w, btext, textt
    #  SingleMod$w - for single modifiers. Assigned to w, btext, textt
    #  NoMod$w - for keys w/o modifiers
    proc SetupEditorWindow {word opts tail} {
        variable ::tkcon::PRIV
        variable ::tkcon::COLOR
        variable ::tkcon::OPT

        set tw [cTW]
        set w [cW]
        EnsureEditorWindow $tw
        SetupEditorWindowCommon $tw

        if {[string length $word] > 50} {
            wm title $tw "Editor $w.text - ...[string range $word end-48 end]"
        } else {
            wm title $tw "Editor $w.text - $word"
        }

        set btext [c_btext]
        set textt [c_text]

        ::clcon_text::clcon_text $w.text
       
        # $tw.text configure -send_to_lisp 1
        # ::btext::clearHighlightClasses $btext

        $btext configure -wrap [dict get $opts -wrap] \
            -xscrollcommand [list $w.sx set] \
            -yscrollcommand [list $w.sy set] \
            -foreground $COLOR(stdin) \
            -background $COLOR(bg) \
            -insertbackground $COLOR(cursor) \
            -font $::tkcon::OPT(font) -borderwidth 1 -highlightthickness 0 \
            -undo 1
        catch {
            # 8.5+ stuff
            set tabsp [expr {$OPT(tabspace) * [font measure $OPT(font) 0]}]
            $btext configure -tabs [list $tabsp left] -tabstyle wordprocessor
        }

 
        scrollbar $w.sx -orient h -command [list $w.text xview]
        scrollbar $w.sy -orient v -command [list $w.text yview]
        
        foreach path [list $tw $w $btext $textt] {
            SetEditorBindtags $path $w
        }

        RebuildMenu  
        
        # Layout
        foreach slave [grid slaves $w] {
            grid remove $slave
        }
        
        grid $btext - $w.sy -sticky news
        grid $w.sx - -sticky ew
        grid columnconfigure $w 0 -weight 1
        grid columnconfigure $w 1 -weight 1
        grid rowconfigure $w 0 -weight 1

        
        LoadContents $w $word $opts $tail

        if {[dict get $opts -type] == "file"} {
            ::recent::AddRecent $word
        }
    }

    proc SyncCursor {text} {
        puts "Text Index Insert = [$text index insert]"
        clcon_text::CallOduvanchikFunction $text "odu::sync-cursor-command nil"
    }
    
    # When we allow for several windows, we will have to keep correspondence
    # between widgets and windows. E.g. by namespace relationship? 
    proc w2tw {w} {
        return $w
    }

    # See docs at the beginning of file
    proc edit {args} {

        set ParsedArgs [EditorParseArgs $args]

        foreach {word opts tail} $ParsedArgs break
        
        # In the future, tw will stand for window, w - for frame
        # Now they coincide

        
        # Find old edit window if there is one
        set Bi [FindOrMakeEditorWindow $word $opts $tail]
        set w [Bi2W $Bi]
        set tw [Bi2TW $Bi]

        ShowExistingBuffer $Bi
       
        if {[string compare [dict get $opts -find] {}]} {
            ::fndrpl::OldTkconFind $w.text [dict get $opts -find] -case 1
        }
        if {[dict get $opts -offset] ne {}} {
            $w.text mark set insert [dict get $opts -offset]
            $w.text see insert
        }

        return $w.text
    }

    proc oImplementation {commandNameWoPrefix} {
        set fn [string cat "odu::" $commandNameWoPrefix "-command"]
        set w [CurrentlyVisibleBuffer]
        set txt $w.text
        set cmd [list clcon_text::CallOduvanchikFunction $txt "$fn nil"]
        set wcmd [clcon_text::WrapEventScriptForFreezedText $cmd $txt]
        eval $wcmd
    }   
}

