## Editor internal buffer lists; mapping between widget structure and buffer id. 

namespace eval ::edt {
    
    # always be global
    variable ReuseCounter

    # when we allow for several editor windows, this variable will be window-local
    # Each element is a dict with keys and values:
    # name - name of window (of kind buf<NN>)
    # type - type (file, var, proc, error)
    # path - path to a file (with a name)
    # w - window
    variable EditorMRUWinList

    # will always be global. Maps contents key (canonicalized edit args)
    # to buffer id. 
    variable ContentKeyToBufIdDict

    # when we allow for several editor windows,
    # will split into global window counter and per window frame counter
    variable EditorWindowCounter

   
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
    

    proc RemoveWindowFromLists {tw w} {
        variable EditorMRUWinList
        variable ContentKeyToBufIdDict
        set i 0
        foreach p $EditorMRUWinList {
            set window [dict get $p w]
            if {$window eq $tw} {
                set EditorMRUWinList [lreplace $EditorMRUWinList $i $i]
                break
            }
            incr i
        }
        dict for {key window} $ContentKeyToBufIdDict {
            if {$window eq $tw} {
                set ContentKeyToBufIdDict [dict remove $ContentKeyToBufIdDict $key]
                break
            }
        }
    }


    # Store window name for buffer list window
    proc AddToWindowLists {key w} {
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
        
        lappend EditorMRUWinList [dict create name $name type $ty path $word w $w]
        dict set ContentKeyToBufIdDict $key $w
    }

    # Returns a window 
    proc FindOrMakeEditorWindow {word opts tail} {
        variable ContentKeyToBufIdDict
        set key [CanonicalizeEditArgs $word $opts]
        if { [dict exists $ContentKeyToBufIdDict $key] } {
            return [dict get $ContentKeyToBufIdDict $key]
        }
        # If not, create one
        set tw [GenEditorWindowName]
        set w $tw
        EnsureEditorWindow $tw
        AddToWindowLists $key $w
        SetupEditorWindow $tw $w $word $opts $tail
        return $tw
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

    
    InitEditorWindowLists

}

