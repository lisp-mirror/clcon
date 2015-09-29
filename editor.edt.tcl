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
    
    proc FindSourceCommand {text} {
        set console [::tkcon::CurrentConsole]
        ::clcon_text::CallOduvanchikFunction $text "find-source-command" {{
            ::edt::FindSourceContinuation $clcon_text $EventAsList
        }}
    }    
    
    # Wrapped for freezed text, for menu only
    proc wesppt {script} {
        ::clcon_text::WrapEventScriptForFreezedText $script [uplevel 1 {string cat "$w.text"}]
    }
    
    proc OduFnMenuItem {w m text oduCmd {accel {}}} {
        set oduFn [string cat $oduCmd "-command"]
        set cmd [wesppt [list clcon_text::CallOduvanchikFunction $text $oduFn]]
        $m add command -label $oduCmd -accel $accel -command $cmd
        if {$accel ne {}} {
            bind $w $accel $cmd
        }
        return $cmd
    }

    proc MakeLispModeMenu {menu w text} {
        set m [menu [::tkcon::MenuButton $menu 3.Lisp lisp]]

        # It is too late hour to start show-mark
        # We have archietectural problems there (rompsite.lisp is too early on the build)
        # set oduCmd "lisp-insert-\)"
        # set cmd [wesppt [list clcon_text::CallOduvanchikFunction $text $oduCmd]]
        # $m add command -label $oduCmd -accel "F11" -command $cmd
        # bind $w <F11> $cmd

        $m add separator
        set cmd [OduFnMenuItem $w $m $text indent-new-line "<Shift-Return>"]
        bind $text <Shift-Return> "$cmd; break"
        OduFnMenuItem $w $m $text transpose-forms
        $m add separator
        OduFnMenuItem $w $m $text beginning-of-defun
        OduFnMenuItem $w $m $text end-of-defun
        OduFnMenuItem $w $m $text mark-defun 
        $m add separator
        OduFnMenuItem $w $m $text forward-form
        OduFnMenuItem $w $m $text backward-form
        OduFnMenuItem $w $m $text forward-list
        OduFnMenuItem $w $m $text backward-list
        OduFnMenuItem $w $m $text forward-up-list
        OduFnMenuItem $w $m $text backward-up-list
        OduFnMenuItem $w $m $text down-list

        $m add separator

        set cmd [wesppt [list ::edt::FindSourceCommand $text]]
        $m add command -label "Find Source" -accel "Alt-." -command $cmd
        bind $w <Alt-period> $cmd
        bind $w <Alt-Key-Cyrillic_yu> $cmd

    }



    proc LoadContents {w word opts tail} {
        switch -glob -- [dict get $opts -type] {
            proc*	{
                $w.text insert 1.0 \
                    [::tkcon::EvalOther {} slave dump proc [list $word]]
                after idle [::tkcon::Highlight $w.text tcl]
            }
            var*	{
                $w.text insert 1.0 \
                    [::tkcon::EvalOther {} slave dump var [list $word]]
                after idle [::tkcon::Highlight $w.text tcl]
            }
            file	{

                if {$word ne {}} {
                    set filemtime [file mtime $word]
                } else {
                    set filemtime {}
                }
                
                $w.text configure -filename $word -filemtime $filemtime
                
                ::clcon_text::ConstructBackendBuffer $w.text
                
                $w.text insert 1.0 [::edt::ReadFileIntoString $word 0]
                
                after idle [::tkcon::Highlight $w.text \
                                [string trimleft [file extension $word] .]]
            }
            error*	{
                $w.text insert 1.0 [join $tail \n]
                after idle [::tkcon::Highlight $w.text error]
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
        
        set txt [::clcon_text::clcon_text $w.text]
        # $w.text configure -send_to_lisp 1
        ::ctext::clearHighlightClasses $w.text

        # set txt [text $w.text]
        
        $w.text configure -wrap [dict get $opts -wrap] \
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
            $w.text configure -tabs [list $tabsp left] -tabstyle wordprocessor
        }

        scrollbar $w.sx -orient h -command [list $w.text xview]
        scrollbar $w.sy -orient v -command [list $w.text yview]
        
        set menu [menu $w.mbar]
        $w configure -menu $menu
        set text $w.text
        
        ## File Menu
        ##
        set m [menu [::tkcon::MenuButton $menu 1.File file]]

        set cmd [wesppt [list ::edt::Save $w.text]]
        $m add command -label "Save" -command $cmd -accel "Control-S"
        bind $w <Control-Key-s> $cmd
        bind $w <Control-Key-Cyrillic_yeru> $cmd
        
        $m add command -label "Save As..."  -underline 0 \
            -command [wesppt [list ::tkcon::Save {} widget $w.text]]
        $m add separator

        set CloseFile [wesppt [list ::edt::EditCloseFile $tw $w]]
        $m add command -label "Close" -accel "Control-w" -command $CloseFile
        bind $w <Control-Key-w> $CloseFile
        
        set dismiss [wesppt [list wm withdraw $tw]]
        $m add command -label "Hide editor window" -underline 0 -command $dismiss
        $m add command -label "4.Reload some of IDE sources" -underline 0 \
	    -command ::tkcon::ReloadSomeIDESources

        
        ## Edit Menu
        ##
        set m [menu [::tkcon::MenuButton $menu 2.Edit edit]]
        $m add command -label "Cut"   -under 2 \
            -command [wesppt [list tk_textCut $text]]
        $m add command -label "Copy"  -under 0 \
            -command [wesppt [list tk_textCopy $text]]
        $m add command -label "Paste" -under 0 \
            -command [wesppt [list tk_textPaste $text]]
        ##
        $m add separator
        $m add command -label "Find" -under 0 \
            -command [wesppt [list ::fndrpl::OpenFindBox $text "text" "find" {}]]

        ## Lisp mode Menu
        ##
        MakeLispModeMenu $menu $w $text
        
        ## Send To Menu
        ## 
        # Try to keep Send menu by allowing to send to main interpreter only
        set m [menu [::tkcon::MenuButton $menu "Send to..." send]]
        set other [tkcon attach]

        set SendToOther [wesppt "::tkcon::EvalOther $other \
		    eval \[$w.text get 1.0 end-1c\]"]
        $m add command -label "Send To [lindex $other 0]" \
            -command $SendToOther

        ## Window Menu
        ##
        set m [menu [::tkcon::MenuButton $menu "7.Window" window]]
        set cmd [list ::clconcmd::bufferlist]
	$m add command -label "Buffer list" -underline 0 -accel "Control-F12" \
            -command $cmd
        bind $w <Control-Key-F12> $cmd
        #
        set cmd [list ::tkcon::FocusConsole]
	$m add command -label "Console" -underline 0 -accel "Control-." \
            -command $cmd
        bind $w <Control-Key-period> $cmd
        
        ## Secret Menu
        ##
        set text $w.text
        set m [menu [::tkcon::MenuButton $menu Secret secret]]

        set oduFn "indent-new-line-command"
        set cmd [wesppt [list clcon_text::CallOduvanchikFunction $text $oduFn]]
        $m add command -label $oduFn -accel "F7" -command $cmd
        bind $w <F7> $cmd
        
        set cmd [list $text Unfreeze]
        $m add command -label "1.Unfreeze (if oduvanchik hang)" -command $cmd

        set cmd [list ::tkcon::EvalInSwankAsync "(clco::compare-clcon_text-and-oduvanchik-buffer-contents \"$text\")" {} {:find-existing}]
        $m add command -label "Check Oduvanchik Sync" -accel "F8" -command $cmd
        bind $w <F8> $cmd

        set cmd [wesppt [list clcon_text::CallOduvanchikFunction $text "nop"]]
        $m add command -label "Sync cursor" -accel "F9" -command $cmd
        bind $w <F9> $cmd

        
        # Layout
        grid $w.text - $w.sy -sticky news
        grid $w.sx - -sticky ew
        grid columnconfigure $w 0 -weight 1
        grid columnconfigure $w 1 -weight 1
        grid rowconfigure $w 0 -weight 1

        
        LoadContents $w $word $opts $tail
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
        putd "We should have reordered windows here"
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
        # focus $w.text.t 

        # this is for text
        focus $w.text
        
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

    proc oImplementation {commandNameWoPrefix} {
        set fn [string cat $commandNameWoPrefix "-command"]
        set w [CurrentlyVisibleBuffer]
        set txt $w.text
        set cmd [list clcon_text::CallOduvanchikFunction $txt $fn]
        set wcmd [clcon_text::WrapEventScriptForFreezedText $cmd $txt]
        eval $wcmd
    }   
    

}

