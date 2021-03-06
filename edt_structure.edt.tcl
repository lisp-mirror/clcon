## Editor internal buffer lists; mapping between widget structure and buffer id. 

namespace eval ::edt {
    
    # always be global
    variable ReuseCounter

    # when we allow for several editor windows, this variable will be window-local
    # Each element is a dict with keys and values:
    # name - title of a window 
    # type - type (file, var, proc, error)
    # path - path to a file (with a name) or a full identifier of proc or error
    # Bi - buffer id
    # -- could also contain a window, but we now have only one editor window.
    variable EditorMRUWinList

    # will always be global. Maps contents key (canonicalized edit args)
    # to Bi (buffer id). 
    variable ContentKeyToBufIdDict

    # when we allow for several editor windows,
    # will split into global window counter and per window frame counter
    variable EditorWindowCounter 

    # current buffer id - Bi of currently active buffer
    variable internal_cBi

    proc cBi {} {
        variable internal_cBi
        checkValidBi $internal_cBi
        return $internal_cBi
    }

    proc checkValidBi {Bi} {
        if {$Bi eq {}} {
            return
        } elseif {![regexp {^buf[0-9]+} $Bi]} {
            puts stderr "wriong Bi $Bi"
            idebug on
            idebug break
            idebug off
        }
    }

    # Returns toplevel editor window for a give buffer id. Currenty we
    # only have a single editor window. See also grep for -w editorry
    proc Bi2TW {Bi} {
        return [::ide_structure::EditorToplevelWindowName]
    }

    # Returns tab entry (notebook slave window) for a given Bi.
    proc Bi2W {Bi} {
        variable ::tkcon::PRIV
        checkValidBi $Bi
        if {$Bi eq {}} {
            return ""
        } else {
            return [string cat [theNotebook] "." $Bi]
        }
    }

    proc Bi2WStatusBar {Bi} {
        return [string cat [Bi2W $Bi] .sb]
    }

    proc W2Bi {w} {
        if {[winfo parent $w] ne [theNotebook]} {
            error "$w must have been a name of window in a notebook"
        }
        lindex [split $w .] end
    }
               

    # returns clcon_text (of class btext)
    proc Bi2btext {Bi} {
        checkValidBi $Bi
        return [string cat [Bi2W $Bi] ".text"]
    }

    # Avoid it. Start from Bi everywhere.  
    #proc clcon_text2Bi {clcon_text} {
    #    if {[winfo class $clcon_text] ne "Btext"]} {
    #        error "$clcon_text must be of class Btext"
    #    }
    #lindex [split $clcon_text .] end-1
    #}

    # returns text embedded into clcon_text
    proc Bi2_text {Bi} {
        checkValidBi $Bi
        return [string cat [Bi2W $Bi] ".text.t"]
    }

    # This can be required when doing bindings on text itself
    proc CoerceTextToItsBText {btext_or_btext_dot_t} {
        set x $btext_or_btext_dot_t
        if {[string range $x end-6 end] eq ".text.t"} {
            return [string range $x 0 end-2]
        } elseif {[string range $x end-4 end] eq ".text"} {
            return $x
        } else {
            puts stderr "Unexpected btext_or_btext_dot_t = $x. If you were using WrapEventScriptForFreezedText, try -destination"
            idebug on
            idebug break
        }
    }
    
    
    # current editing widget (e.g. frame, but now - just window)
    proc cW {} {
        variable internal_cBi
        return [Bi2W $internal_cBi]
    }

    proc cTW {} {
        error "Use theTW instead"
    }
    
    proc theTW {} {
        return [::ide_structure::EditorToplevelWindowName]
    }

    proc theNotebook {} {
        string cat [theTW] .frammy
    }

    # returns current clcon_text == btext
    proc c_btext {} {
        variable internal_cBi
        return [Bi2btext $internal_cBi]
    }

    proc c_text {} {
        variable internal_cBi
        return [Bi2_text $internal_cBi]
    }

   
    # Reuse counter increments when we open a non-reusable window
    # This is required to know which window was opened earlier
    proc GenReuseCounter {} {
        variable ReuseCounter
        if {![info exists ReuseCounter]} {
            set ReuseCounter 0
        }
        return [incr ReuseCounter]
    }

    # We canonicalize edit args to key and then
    # Store window into ContentKeyToBufIdDict by its key. 
    # And we store it in EditorWindowList for selection (ordered by usage history)

    # We have global variables to keep window list
    proc InitEditorWindowLists {} {
        variable EditorMRUWinList
        variable ContentKeyToBufIdDict
        if {![info exists EditorMRUWinList]} {
            variable EditorWindowCounter
            variable internal_cBi
            set internal_cBi ""
            set EditorMRUWinList [list]
            set ContentKeyToBufIdDict [dict create]
            set EditorWindowCounter 0
        }
    }

    # This function can be useful for some other tools, e.g. recent files menu
    proc IsFileBeingEdited {filename} {
        variable ContentKeyToBufIdDict
        set key [list [CanonicalizeFileName $filename] -type file]
        dict exists $ContentKeyToBufIdDict $key
    }

    # Returns list of index and entry of EditorMRUWinList for buffer id Bi
    proc SearchBiInMRUWinList {Bi} {
        variable EditorMRUWinList
        set i 0
        foreach p $EditorMRUWinList {
            set thisBi [dict get $p "Bi"]
            if {$thisBi eq $Bi} {
                return [list $i $p]
            }
            incr i
        }
        return [list -1 {}]
    }

    proc RemoveWindowFromLists {Bi} {
        variable EditorMRUWinList
        variable ContentKeyToBufIdDict
        set i [lindex [SearchBiInMRUWinList $Bi] 0]
        set EditorMRUWinList [lreplace $EditorMRUWinList $i $i]
        dict for {key thisBi} $ContentKeyToBufIdDict {
            if {$thisBi eq $Bi} {
                set ContentKeyToBufIdDict [dict remove $ContentKeyToBufIdDict $key]
                break
            }
        }
    }


    # Store window name for buffer list window
    proc AddToWindowLists {key Bi} {
        variable EditorMRUWinList
        variable ContentKeyToBufIdDict

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
        
        lappend EditorMRUWinList [dict create name $name type $ty path $word Bi $Bi]
        dict set ContentKeyToBufIdDict $key $Bi
    }

    # generates new, unique Bi. 
    proc GenNewBi {} {
        variable EditorWindowCounter
        incr EditorWindowCounter
        set result [string cat "buf" $EditorWindowCounter]
        return $result
    }


    # Reorganizes windows according to their usage order
    # Refreshes buffer list. LastUsedTw can be {} when deleting window!
    proc UpdateMRUAndBufferList {LastUsedBi} {
        variable EditorMRUWinList
        lassign [SearchBiInMRUWinList $LastUsedBi] index entry
        if {$index>=0} {
            set x [lreplace $EditorMRUWinList $index $index]
            set EditorMRUWinList [linsert $x 0 $entry]
        }
        #putd "We should have reordered windows here. See recent.tcl"
        after idle ::buli::RefreshData
    }


    proc PerformSwitchToBufferAction {action} {
        set tw [theTW]
        switch -exact $action {
            "" -
            focus {
                ::gui_util::FocusWindowByName $tw [c_text]
            }
            deiconify {
                wm deiconify $tw
            }
            do_nothing {
                # do nothing
            }
            default {
                error "PerformSwitchToBufferAction: wrong action $action"
            }
        }
    }

    # Switch to existing buffer identified by buffer id, or
    # reflect the fact that user switchted to that tab
    # action is "", "focus", "deiconify" or "do_nothing", see PerformSwitchToBufferAction.
    # "" is an equialent to "focus"
    # Action specified is performed only if we do not need to change a tab.
    # If we switch to another tab, action is always "focus"
    proc SwitchToBuffer {Bi {action "focus"}} {
        set nb [theNotebook]
        set w [Bi2W $Bi]
        if {[$nb select] eq $w} {
            event generate $nb <<NotebookTabChanged>> -data $action
        } else {
            # We can not pass user data to that event, so
            # its action will be "", that is, "focus"
            [theNotebook] select [Bi2W $Bi]
        }
        ::edt::MaybeAddToRecentWhenClosingOrSwitching $Bi
    }

    ## event generate [theNotebook] <???> -data string
    ## %d
    
    # Calls to set things up when existing buffer is shown at the tab.
    # Buffer is identified by internal_cBi
    # Does not check existence
    # It is caller responsibility to ensure that buffer is visible
    proc ShowingBufferChanged {} {

        set w [cW]
        set tw [theTW]
        
        SetupEditorWindowWhenSwitchingToIt

        UpdateMRUAndBufferList [cBi]
    }

    proc EnsureSingleEditorWindowExists {} {
        set tw [Bi2TW asdfasdf]
        if {![winfo exists $tw]} {
            EnsureToplevelWindowWithPathname $tw
            SetupEditorWindowCommon $tw
            ::win_lay::PositionATool $tw
        }
    }

    
    # Word is filename,procname, etc.
    # Opts are options from edit command
    # Tail is as returned from EditorParseArgs 
    # Sets internal_cBi and returns a list of Bi and canonicalized word    # 
    proc FindOrMakeEditorWindow {word opts tail} {
        variable ContentKeyToBufIdDict
        variable internal_cBi
        set key [CanonicalizeEditArgs $word $opts]
        set word [lindex $key 0]
        if { [dict exists $ContentKeyToBufIdDict $key] } {

            set Bi [dict get $ContentKeyToBufIdDict $key]
            checkValidBi $Bi
            set internal_cBi $Bi

        } else {
            # If not, create one
            set Bi [GenNewBi]

            checkValidBi $Bi
            set internal_cBi $Bi
            
            AddToWindowLists $key $Bi

            set tw [theTW]
            set w [cW]

            EnsureSingleEditorWindowExists
            
            SetupEditorWindowWhenCreatedBuffer $opts

            # SetupEditorWindowWhenSwitchingToIt

            LoadContentsAndUpdateRecent $w $word $opts $tail
        }

        return [list $internal_cBi $word]
    }

    proc CurrentlyVisibleBi {} {
        return [cBi]
    }
    
    # returns Window. Assume window is always visible. 
    proc CurrentlyVisibleBuffer {} {
        return [cW]
    }

    # Is called when we determine that oduvanchik disconnected
    proc OduvanchikDisconnected {} {
        variable EditorMRUWinList
        foreach p $EditorMRUWinList {
            set Bi [dict get $p "Bi"]
            set btext [Bi2btext $Bi]
            catch { ::clcon_text::OduvanchikDisconnected_clcon_text $btext } 
        }            
    }

    
    InitEditorWindowLists

}

