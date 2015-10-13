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
        if {![regexp {^buf[0-9]+} $Bi]} {
            puts stderr "wriong Bi $Bi"
            idebug on
            idebug break
            idebug off
        }
    }

    proc Bi2W {Bi} {
        variable ::tkcon::PRIV
        checkValidBi $Bi
        if {$Bi eq {}} {
            return ""
        } else {
            return [string cat $PRIV(base). $Bi]
        }
    }

    proc Bi2TW {Bi} {
        checkValidBi $Bi
        return [Bi2W $Bi]
    }


    proc Bi2btext {Bi} {
        checkValidBi $Bi
        return [string cat [Bi2W $Bi] ".text"]
    }

    proc Bi2_text {Bi} {
        checkValidBi $Bi
        return [string cat [Bi2W $Bi] ".text.t"]
    }
    
    
    # current editing widget (e.g. frame, but now - just window)
    proc cW {} {
        variable internal_cBi
        return [Bi2W $internal_cBi]
    }

    # current editing window (not a frame)
    proc cTW {} {
        variable internal_cBi
        return [Bi2TW $internal_cBi]
    }

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
        set key [list $filename -type file]
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
    proc UpdateMRUAndBufferList {LastUsedTw} {
        putd "We should have reordered windows here. See recent.tcl"
        after idle ::buli::RefreshData
    }

    # Switch to existing buffer identified by buffer id.
    proc SwitchToBuffer {Bi} {
        variable internal_cBi
        checkValidBi $Bi
        set internal_cBi $Bi
        ShowExistingBuffer        
    }
    
    # Shows buffer identified by internal_cBi
    # Does not check existence
    proc ShowExistingBuffer {} {

        set w [cW]
        set tw [cTW]
        
        HideAllEditorWindows
        wm deiconify $tw

        focus [c_text]
       
        UpdateMRUAndBufferList [cBi]
    }

    
    # Word is filename,procname, etc.
    # Opts are options from edit command
    # Tail is as returned from EditorParseArgs 
    # Sets internal_cBi and returns it
    proc FindOrMakeEditorWindow {word opts tail} {
        variable ContentKeyToBufIdDict
        variable internal_cBi
        set key [CanonicalizeEditArgs $word $opts]
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

            set tw [cTW]
            set w [cW]
            EnsureEditorWindow $tw
            SetupEditorWindowCommon $tw
            
            SetupEditorWindow $opts 

            LoadContentsAndUpdateRecent $w $word $opts $tail
        }

        return $internal_cBi
    }

    proc CurrentlyVisibleBi {} {
        return [cBi]
    }
    
    # returns Window. Assume window is always visible. 
    proc CurrentlyVisibleBuffer {} {
        return [cW]
    }

    
    InitEditorWindowLists

}

