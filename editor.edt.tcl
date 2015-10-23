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
        ::tkcon::FocusWindowByName [cTW] [c_text]
        return 
    }

    proc AnyBufferBi {} {
        variable EditorMRUWinList
        set p [lindex $EditorMRUWinList 0]
        if {$p eq {}} {
            return {}
        } else {
            return [dict get $p "Bi"]
        }
    }
    
    # close file (without saving for now)  
    proc EditCloseFile {Bi} {
        set w [Bi2W $Bi]
        putd "Saving file if omitted!" 
        RemoveWindowFromLists $Bi
        # UpdateMRUAndBufferList {}
        set newBi [AnyBufferBi]
        if {$newBi eq {}} {
            # no more buffers - let's kill the editor window
            after idle [list destroy [cTW]]
        } else {
            SwitchToBuffer $newBi do_nothing
            after idle [list destroy $w]
        }
    }

    # Wrapped for freezed text, for menu only
    proc wesppt {script args} {
        named_args $args {-add-break 0}
        ::clcon_text::WrapEventScriptForFreezedText $script -destination [uplevel 1 {string cat "$w.text"}] -add-break $(-add-break)
    }

    proc LoadContentsAndUpdateRecent {w word opts tail} {
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

                ::recent::AddRecent $word
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

    proc TextModified {Bi} {
        set textt [Bi2_text $Bi]
        set modified [$textt edit modified]
        set w [Bi2W $Bi]
        set notebook [theNotebook]
        set index [$notebook index $w]
        ::buli::RefreshData
        $notebook tab $index -text [CalcTabText $Bi]
    }

    proc CalcTabText {Bi} {
        set MRUWinListEntry [lindex [SearchBiInMRUWinList [cBi]] 1]
        set tab_name [dict get $MRUWinListEntry name]
        set btext [Bi2btext $Bi]
        set asterik [BooleanToAsterik [$btext edit modified]]
        string cat $tab_name $asterik        
    }

    # Init buffer GUI when it is first created 
    # variable internal_cBi is set already
    # Args:
    # opts - editor options
    proc SetupEditorWindowWhenCreatedBuffer {opts} {
        variable ::tkcon::PRIV
        variable ::tkcon::COLOR
        variable ::tkcon::OPT

        set Bi [cBi]
        set tw [cTW]
        set w [cW]
        set WStatusBar [Bi2WStatusBar $Bi]
        set btext [c_btext]
        set textt [c_text]
        set notebook [theNotebook]

        frame $w
        ::clcon_text::clcon_text $btext


        frame $WStatusBar
        label $WStatusBar.cursor -relief sunken -borderwidth 1 -anchor e -width 6 \
	    -textvariable $btext.CursorPos
        grid $WStatusBar.cursor -sticky news         

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

        bind $btext <<Modified>> [list ::edt::TextModified $Bi]

        scrollbar $w.sx -orient h -command [list $w.text xview]
        scrollbar $w.sy -orient v -command [list $w.text yview]

        grid $btext -row 0 -column 0 -sticky nsew
        grid $w.sy -row 0 -column 1 -sticky ns
        grid $w.sx -row 1 -column 0 -sticky ew
        grid $WStatusBar -row 2 -column 0 -columnspan 2 -sticky ew
        
        grid columnconfigure $w 0 -weight 1
        # grid columnconfigure $w 1 -weight 1
        grid rowconfigure $w 0 -weight 1

        set tab_name [CalcTabText [cBi]]
        $notebook add $w -text $tab_name
    }

    
    # Init buffer GUI when switching to it
    # variable internal_cBi is set already
    # Args:
    # opts - editor options
    # Important variables:
    #  btext - clcon_text widget (currently is a btext wrapper)
    # Bindtags:
    #  DoubleKey$w - for double modifiers. Assigned to w, btext, textt
    #  SingleMod$w - for single modifiers. Assigned to w, btext, textt
    #  NoMod$w - for keys w/o modifiers
    proc SetupEditorWindowWhenSwitchingToIt {} {
        variable ::tkcon::PRIV
        variable ::tkcon::COLOR
        variable ::tkcon::OPT

        set MRUWinListEntry [lindex [SearchBiInMRUWinList [cBi]] 1]
        set word [dict get $MRUWinListEntry path]

        set tw [cTW]
        set w [cW]
        set btext [c_btext]
        set textt [c_text]

        if {[string length $word] > 50} {
            wm title $tw "Editor $btext - ...[string range $word end-48 end]"
        } else {
            wm title $tw "Editor $btext - $word"
        }

        foreach path [list $tw $w $btext $textt] {
            SetEditorBindtags $path $w
        }

        RebuildMenu

        EnableDisableMenuItems

    }

    proc SwitchToThisTab {{action "focus"}} {
        variable internal_cBi

        set notebook [theNotebook]
        set Bi [W2Bi [$notebook select]]

        checkValidBi $Bi
        set internal_cBi $Bi

        # We might avoid this call if the new that no switch is actually done,
        ShowingBufferChanged

        after idle [list ::edt::PerformSwitchToBufferAction $action]
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
        set btext [Bi2btext $Bi]

        # This is partially async command, not sure see insert would work now
        SwitchToBuffer $Bi focus

        if {[string compare [dict get $opts -find] {}]} {
            ::fndrpl::OldTkconFind $btext [dict get $opts -find] -case 1
        }
        if {[dict get $opts -offset] ne {}} {
            $btext mark set insert [dict get $opts -offset]
            $btext see insert
        }

        return $btext
    }

    proc oImplementation {commandNameWoPrefix} {
        set fn [string cat "odu::" $commandNameWoPrefix "-command"]
        set w [CurrentlyVisibleBuffer]
        set txt $w.text
        set cmd [list clcon_text::CallOduvanchikFunction $txt "$fn nil"]
        set wcmd [clcon_text::WrapEventScriptForFreezedText $cmd -destination $txt]
        eval $wcmd
    }   
}

