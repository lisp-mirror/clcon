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
#   BROKEN -attach interpreter - get data in that interpreter 
#   type	proc/file/var
#   options 
# Returns:	nothing
# Added by budden : -offset option, accepts index
##

namespace eval ::edt {
    
    # always be global
    variable ReuseCounter

    # when we allow for several editor windows, this variable will be window-local
    # Each element is a dict with keys and values:
    # name - name of window
    # type - type (file, var, proc, error)
    # path - path to a file (with a name)
    # w - window
    variable EditorMRUWinList

    # will always be global 
    variable EditorReusableWindowDict

    # when we allow for several editor windows,
    # will split into global window counter and per window frame counter
    variable EditorWindowCounter

   
    # This will be an option
    # If true, we allow for only one editor window at a time, joungling frames in it
    # New window to the same place where old one was
    proc SingleEditorWindow {} {
        return 1
    }

    # We canonicalize edit args to key and then
    # Store window into EditorReusableWindowDict by its key. 
    # And we store it in EditorWindowList for selection (ordered by usage history)

    # Reuse counter increments when we open a non-reusable window
    # This is required to know which window was opened earlier
    proc GenReuseCounter {} {
        variable ReuseCounter
        if {![info exists ReuseCounter]} {
            set ReuseCounter 0
        }
        return [incr ReuseCounter]
    }


    # This function increases ReuseCounter for non-reusable parameter sets
    # Take care to call it once for every parameter set.
    proc CanonicalizeEditArgs {word opts} {
        set type [dict get $opts -type]
        if {$type eq "file"} {
            return [list $word -type $type]
        } elseif {$type eq "error"} {
            return [list tcl_error -type $type -no [GenReuseCounter]]
        } else {
            # never reuse procs, vars, errors
            return [list $word -type $type -no [GenReuseCounter]]
        }
    }


    # We have global variables to keep window list
    proc InitEditorWindowLists {} {
        variable EditorMRUWinList
        variable EditorReusableWindowDict
        if {![info exists EditorMRUWinList]} {
            variable EditorWindowCounter
            set EditorMRUWinList [list]
            set EditorReusableWindowDict [dict create]
            set EditorWindowCounter 0
        }
    }

    InitEditorWindowLists

    proc GenEditorWindowName {} {
        variable ::tkcon::PRIV
        variable EditorWindowCounter
        incr EditorWindowCounter
        set result [string cat $PRIV(base).__edit $EditorWindowCounter]
        set SomeEditorWindowName $result
        return $result
    }

    proc ShowSomeEditor {} {
        set tw [CurrentlyVisibleBuffer]
        ::tkcon::FocusWindowByName $tw $tw.text
        return 
    }

    # returns Window. When we use multiple frames, would return frame
    proc CurrentlyVisibleBuffer {} {
        variable EditorMRUWinList
        foreach p $EditorMRUWinList {
            set window [dict get $p w]
            if {[winfo exists $window] && [winfo ismapped $window]} {
                return $window
            }
        }
        return {}
    }

    proc EncodeTypeForBufferList {type} {
        switch -exact $type {
            file {return "f"}
            proc {return "p"}
            var {return "v"}
            error {return "e"}
            default {return "?"}
        }
    }

    # Store window name for buffer list window
    proc AddToWindowLists {key w} {
        variable EditorMRUWinList
        variable EditorReusableWindowDict

        set word [lindex $key 0]
        set options [lrange $key 1 end]
        set type [dict get $options -type]
        if {[dict exists $options -no]} {
            set no [dict get $options -no]
        } else {
            set no ""
        }

        set ty [EncodeTypeForBufferList $type]

        if {$type eq {file}} {
            set name [lindex [file split $word] end]
        } else {
            set name "$word $no"
        }
        
        lappend EditorMRUWinList [dict create name $name type $ty path $word w $w]
        dict set EditorReusableWindowDict $key $w
    }

    # This function can be useful for some other tools, e.g. recent files menu
    proc IsFileBeingEdited {filename} {
        variable EditorReusableWindowDict
        set key [list $filename -type file]
        dict exists $EditorReusableWindowDict $key
    }
    
    # RuseOrCreateEditorWindow . Returns a window 
    proc FindOrMakeEditorWindow {word opts tail} {
        variable EditorReusableWindowDict
        set key [CanonicalizeEditArgs $word $opts]
        if { [dict exists $EditorReusableWindowDict $key] } {
            return [dict get $EditorReusableWindowDict $key]
        }
        # If not, create one
        set tw [GenEditorWindowName]
        set w $tw
        EnsureEditorWindow $tw
        AddToWindowLists $key $w
        SetupEditorWindow $tw $w $word $opts $tail
        return $tw
    }

    proc HideAllEditorWindows {} {
        variable EditorMRUWinList
        foreach p $EditorMRUWinList {
            set window [dict get $p w]
            if {[winfo exists $window]} {
                wm withdraw $window
            }
        }
    }

    proc RemoveWindowFromLists {tw w} {
        variable EditorMRUWinList
        variable EditorReusableWindowDict
        set i 0
        foreach p $EditorMRUWinList {
            set window [dict get $p w]
            if {$window eq $tw} {
                set EditorMRUWinList [lreplace $EditorMRUWinList $i $i]
                break
            }
            incr i
        }
        dict for {key window} $EditorReusableWindowDict {
            if {$window eq $tw} {
                set EditorReusableWindowDict [dict remove $EditorReusableWindowDict $key]
                break
            }
        }
    }

    # close file (without saving for now) and open MRU 
    proc EditCloseFile {tw w} {
        if {[winfo exists $tw]} {
            wm withdraw $tw
        }
        putd "Saving file if omitted!" 
        RemoveWindowFromLists $tw $w
        destroy $w
        UpdateMRUAndBufferList {}
        ::recent::RedrawRecentMenuForConsole
    }

    proc MaybeDestroyEditorWindow {tw} {
        variable EditorMRUWinList
        if {![llength $EditorMRUWinList]} {
            wm destroy $tw
        }
    }
        
    # Hides editor window. 
    # If EditorMRUWinList is empty, destroys it
    proc HideEditorWindow {tw} {
        putd "Normally hiding editor windows assumes closing all files. We are wrong"
        wm withdraw $tw
        after idle "::edt::MaybeDestroyEditorWindow $tw"
    }


    proc ReadFileIntoString {word {RemoveLastNewline 0}} {
        set obj [string cat "__tkcon" [GenNamedCounter "ReadFileObj"]]
        if {$RemoveLastNewline} {
            set NoNewLineOption "-nonewline"
        } else {
            set NoNewLineOption ""
        }
        set cmd [subst -nocommands {
            set ${obj}(fid) [open {$word} r]
            set ${obj}(data) [read $NoNewLineOption \$${obj}(fid)]
            close \$${obj}(fid)
            after 1000 unset ${obj}
            return \$${obj}(data)
        }
                ]
        set line [::tkcon::EvalOther {} slave eval $cmd]
        return $line
        # if {$RemoveLastNewline && [string range $line end end] eq "\n"} {
        #     puts "oKi"
        #     return [string range $line 0 end-1]
        # } else {
        #     return $line
        # }
    }

    proc FindSourceContinuation {clcon_text EventAsList} {
        set Head [::mprs::Unleash [lindex $EventAsList 0]]
        ::mprs::AssertEq $Head ":return"
        set l2 [::mprs::Unleash [lindex $EventAsList 1]]
        set h2 [::mprs::Unleash [lindex $l2 0]]
        if {$h2 eq ":ok"} {
            set code [::mprs::Unleash [lindex $l2 1]]
            set proc [subst -nocommand {{w} {$code}}]
            # tk_messageBox -message $proc
            apply $proc [::tkcon::CurrentConsole] 
        } else {
            tk_messageBox -parent $clcon_text -message "FindSource command aborted"
        }
          
    }

    # See also ::tkcon::LispFindDefinition
    proc FindSourceCommand {text} {
        set console [::tkcon::CurrentConsole]
        ::clcon_text::CallOduvanchikFunction $text "odu::find-source-command nil" {{
            ::edt::FindSourceContinuation $clcon_text $EventAsList
        }}
    }    
    
    # Wrapped for freezed text, for menu only
    proc wesppt {script} {
        ::clcon_text::WrapEventScriptForFreezedText $script [uplevel 1 {string cat "$w.text"}]
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

    proc CompileAndLoadTheFile {clcon_text} {
        set opened_file [$clcon_text cget -opened_file]
        set FileName [$opened_file cget -filename]
        set qFileName [::tkcon::QuoteLispObjToString $FileName]
        set form "(clco::compile-file-for-tcl $qFileName t)"
        ::tkcon::FocusConsole
        ::tkcon::SendEventToSwank $form {} 1 {:find-existing}
    }

    proc MakeLispModeMenu {menu w btext} {
        set m [menu [::tkcon::MenuButton $menu 3.Lisp lisp]]

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
        OduFnMenuItem $w $m $btext forward-form
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

    proc e_indent {w {extra "    "}} {
        set lineno [expr {int([$w index insert])}]
        set line [$w get $lineno.0 $lineno.end]
        regexp {^(\s*)} $line -> prefix
        if {[string index $line end] eq "\{"} {
            tk::TextInsert $w "\n$prefix$extra"
        } elseif {[string index $line end] eq "\}"} {
            if {[regexp {^\s+\}} $line]} {
                $w delete insert-[expr [string length $extra]+1]c insert-1c
                tk::TextInsert $w "\n[string range $prefix 0 end-[string length $extra]]"
            } else {
                tk::TextInsert $w "\n$prefix"
            }
        } else {
            tk::TextInsert $w "\n$prefix"
        }
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

    
    # Initializes editor GUI, loads text.
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
    proc SetupEditorWindow {tw w word opts tail} {
        variable ::tkcon::PRIV
        variable ::tkcon::COLOR
        variable ::tkcon::OPT

        if {[string length $word] > 50} {
            wm title $tw "Editor $w.text - ...[string range $word end-48 end]"
        } else {
            wm title $tw "Editor $w.text - $word"
        }

        wm protocol $tw WM_DELETE_WINDOW "::edt::HideEditorWindow $tw"
        
        set btext [::clcon_text::clcon_text $w.text]
        set textt $btext.t
        
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

        foreach path [list $w $btext $textt] {
            bindtags $path [concat DoubleMod$w SingleMod$w NoMod$w [bindtags $path]]
        }

        scrollbar $w.sx -orient h -command [list $w.text xview]
        scrollbar $w.sy -orient v -command [list $w.text yview]
        
        set menu [menu $w.mbar]
        $w configure -menu $menu
        
        ## File Menu
        ##
        set m [menu [::tkcon::MenuButton $menu 1.File file]]

        set cmd ::tkcon::OpenForEdit 
	$m add command -label "Open" -command $cmd -accel "Control-O"
        bind SingleMod$w <Control-Key-o> "$cmd; break"
        bind SingleMod$w <Control-Key-Cyrillic_shcha> "$cmd; break"

        set cmd [wesppt [list ::edt::Save $w.text]]
        $m add command -label "Save" -command $cmd -accel "Control-S"
        bind SingleMod$w <Control-Key-s> $cmd
        bind SingleMod$w <Control-Key-Cyrillic_yeru> $cmd
        
        $m add command -label "Save As..."  -underline 0 \
            -command [wesppt [list ::tkcon::Save {} widget $w.text]]
        $m add separator

        set CloseFile [wesppt [list ::edt::EditCloseFile $tw $w]]
        $m add command -label "Close" -accel "Control-w" -command $CloseFile
        bind SingleMod$w <Control-Key-w> $CloseFile
        
        set dismiss [wesppt [list wm withdraw $tw]]
        $m add command -label "Hide editor window" -underline 0 -command $dismiss
        $m add command -label "4.Reload some of IDE sources" -underline 0 \
	    -command ::tkcon::ReloadSomeIDESources

        
        ## Edit Menu
        ##
        set m [menu [::tkcon::MenuButton $menu 2.Edit edit]]
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
        MakeLispModeMenu $menu $w $btext
        
        ## Tcl mode Menu
        ## 
        set m [menu [::tkcon::MenuButton $menu "4.Tcl" tcl]]
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
        set m [menu [::tkcon::MenuButton $menu "7.Window" window]]
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
        set m [menu [::tkcon::MenuButton $menu Secret secret]]

        set cmd [list $btext Unfreeze]
        $m add command -label "1.Unfreeze (if oduvanchik hang)" -command $cmd

        set cmd [list ::tkcon::EvalInSwankAsync "(clco::compare-clcon_text-and-oduvanchik-buffer-contents \"$btext\")" {} {:find-existing}]
        $m add command -label "Check Oduvanchik Sync" -accel "F8" -command $cmd
        bind NoMod$w <F8> $cmd

        set cmd [wesppt [list ::edt::SyncCursor $btext]]
        $m add command -label "Sync cursor" -accel "F9" -command $cmd
        bind NoMod$w <F9> $cmd

        
        # Layout
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
    
    proc EnsureEditorWindow {tw} {
        if {![winfo exists $tw]} {
            toplevel $tw
            wm withdraw $tw
        }
        return $tw
    }    


    # Parses line and returns list of {word opts tail}
    proc EditorParseArgs {args} {
        variable ::tkcon::PRIV
        variable ::tkcon::COLOR
        variable ::tkcon::OPT

        set args [lindex $args 0]
        
        set opts [dict create -find {} -type {} -attach {} -wrap {none} -offset {}]             
        while {[string match -* [lindex $args 0]]} {
            switch -glob -- [lindex $args 0] {
                -f*	{ dict set opts -find [lindex $args 1] }
                -a*	{ dict set opts -attach [lindex $args 1] }
                -t*	{ dict set opts -type [lindex $args 1] }
                -w*	{ dict set opts -wrap [lindex $args 1] }
                -o* { dict set opts -offset [lindex $args 1] }
                --	{ set args [lreplace $args 0 0]; break }
                default {return -code error "unknown option \"[lindex $args 0]\""}
            }
            set args [lreplace $args 0 1]
        }

        # determine what interpreter we are dealing with (broken)
        # if {[llength [ dict get $opts -attach]]} {
        #     foreach {app type} [ dict get $opts -attach] {break}
        # } else {
        #     foreach {app type} [tkcon attach] {break}
        # }
        
        foreach {app type} [tkcon attach] {break}
        
        set word [lindex $args 0]

        if {[dict get $opts -type] == {}} {
            if {[llength [::tkcon::EvalOther $app $type info commands [list $word]]]} {
                dict set opts -type "proc"
            } elseif {[llength [::tkcon::EvalOther $app $type info vars [list $word]]]} {
                dict set opts -type "var"
            } elseif {[::tkcon::EvalOther $app $type file isfile [list $word]]} {
                dict set opts -type "file"
            }
        }
        if {[dict get $opts -type] == {}} {
            return -code error "unrecognized type '$word'"
        }

        set tail $args

        return [list $word $opts $tail]
    }

    # When we allow for several windows, we will have to keep correspondence
    # between widgets and windows. E.g. by namespace relationship? 
    proc w2tw {w} {
        return $w
    }

    # Reorganizes windows according to their usage order
    # Refreshes buffer list. LastUsedTw can be {} when deleting window!
    proc UpdateMRUAndBufferList {LastUsedTw} {
        putd "We should have reordered windows here. See recent.tcl"
        after idle ::buli::RefreshData
    }
    
    # Shows buffer identified by its w (window; in the future - a frame).
    # Does not check existence
    proc ShowExistingBuffer {w} {

        # Do not remove this, see usages before!
        set tw [w2tw $w]
        
        HideAllEditorWindows
        wm deiconify $tw

        # this is for ctext
        focus $w.text.t 

        # this is for text
        # focus $w.text
        
        UpdateMRUAndBufferList $tw
    }

    # See docs at the beginning of file
    proc edit {args} {

        set ParsedArgs [EditorParseArgs $args]

        foreach {word opts tail} $ParsedArgs break
        
        # In the future, tw will stand for window, w - for frame
        # Now they coincide

        
        # Find old edit window if there is one
        set tw [FindOrMakeEditorWindow $word $opts $tail]
        set w $tw

        ShowExistingBuffer $w
       
        if {[string compare [dict get $opts -find] {}]} {
            ::fndrpl::OldTkconFind $w.text [dict get $opts -find] -case 1
        }
        if {[dict get $opts -offset] ne {}} {
            $w.text mark set insert [dict get $opts -offset]
            $w.text see insert
        }

        return $w.text
    }

    # This should go to special file?
    proc TextSetSelectionTo {text from to} {
        $text tag remove sel 1.0 end
        $text tag add sel $from $to
    }

    # Returns list of two indices
    proc TextSelectionCoordinates {text} {
        $text tag nextrange sel 1.0 end
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

