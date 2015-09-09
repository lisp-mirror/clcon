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
        ::tkcon::FocusWindowByName $tw
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
    

    # Initializes editor GUI, loads text
    # args are for error only
    proc SetupEditorWindow {tw w word opts tail} {
        variable ::tkcon::PRIV
        variable ::tkcon::COLOR
        variable ::tkcon::OPT

        if {[string length $word] > 20} {
            wm title $tw "[string range $word 0 16]... - Edit $w.text"
        } else {
            wm title $tw "$word - Edit $w.text"
        }

        wm protocol $tw WM_DELETE_WINDOW "::edt::HideEditorWindow $tw"
        
        set txt [::clcon_text::clcon_text $w.text]
        $w.text configure -send_to_lisp 1

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
        
        ## File Menu
        ##
        set m [menu [::tkcon::MenuButton $menu File file]]
        $m add command -label "Save As..."  -underline 0 \
            -command [list ::tkcon::Save {} widget $w.text]
        $m add command -label "Append To..."  -underline 0 \
            -command [list ::tkcon::Save {} widget $w.text a+]
        $m add separator

        set CloseFile [list ::edt::EditCloseFile $tw $w]
        $m add command -label "Close" -accel "Control-w" -command $CloseFile
        bind $w <Control-Key-w> $CloseFile
        
        set dismiss [list wm withdraw $tw]
        $m add command -label "Hide editor window" -underline 0 -command $dismiss
        
        ## Edit Menu
        ##
        set text $w.text
        set m [menu [::tkcon::MenuButton $menu Edit edit]]
        $m add command -label "Cut"   -under 2 \
            -command [list tk_textCut $text]
        $m add command -label "Copy"  -under 0 \
            -command [list tk_textCopy $text]
        $m add command -label "Paste" -under 0 \
            -command [list tk_textPaste $text]
        $m add separator
        $m add command -label "Find" -under 0 \
            -command [list ::fndrpl::OpenFindBox $text "text" "find" {}]
        
        ## Send To Menu
        ## 
        # Try to keep Send menu by allowing to send to main interpreter only
        set m [menu [::tkcon::MenuButton $menu "Send to..." send]]
        set other [tkcon attach]
        $m add command -label "Send To [lindex $other 0]" \
            -command "::tkcon::EvalOther $other \
		    eval \[$w.text get 1.0 end-1c\]"

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
        
        
        # Layout
        grid $w.text - $w.sy -sticky news
        grid $w.sx - -sticky ew
        grid columnconfigure $w 0 -weight 1
        grid columnconfigure $w 1 -weight 1
        grid rowconfigure $w 0 -weight 1

        
        
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
                $w.text insert 1.0 [::tkcon::EvalOther {} slave eval \
                                        [subst -nocommands {
                                            set __tkcon(fid) [open {$word} r]
                                            set __tkcon(data) [read \$__tkcon(fid)]
                                            close \$__tkcon(fid)
                                            after 1000 unset __tkcon
                                            return \$__tkcon(data)
                                        }
                                        ]]
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
        $w.text mark set insert 0.0
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
        focus -force $w.text
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
    }

}
