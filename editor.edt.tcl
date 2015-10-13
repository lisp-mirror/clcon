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


    # Init buffer GUI when it is first created 
    # variable internal_cBi is set already
    # Args:
    # opts - editor options
    proc SetupEditorWindowWhenCreatedBuffer {opts} {
        variable ::tkcon::PRIV
        variable ::tkcon::COLOR
        variable ::tkcon::OPT
        
        set tw [cTW]
        set w [cW]
        
        set MRUWinListEntry [lindex [SearchBiInMRUWinList [cBi]] 1]
        set word [dict get $MRUWinListEntry path]

        set btext [c_btext]
        set textt [c_text]

        if {[string length $word] > 50} {
            wm title $tw "Editor $btext - ...[string range $word end-48 end]"
        } else {
            wm title $tw "Editor $btext - $word"
        }
        
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

        set tw [cTW]
        set w [cW]

        set MRUWinListEntry [lindex [SearchBiInMRUWinList [cBi]] 1]
        set word [dict get $MRUWinListEntry path]

        set btext [c_btext]
        set textt [c_text]

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

        ShowExistingBuffer
       
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

