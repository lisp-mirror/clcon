package require Tk
package require tablelist
# TkconSourceHere util.tcl
# SearchState is a dict:
#    -continueP
#       1 - if this is a continuation of search, 0 otherwise
#    -startFrom {}|key
#       {} - start from current position, key - start from that position
#    -direction {forwards}|{backwards}
#    -findcase 
#       1 - case sensitive, 0 - insensitive

namespace eval ::srchtblst {

    proc EnsurePopulated {tbl row} {
        set name [$tbl rowcget $row -name]
        if {$name eq "a"} {
            if {[$tbl childcount $row] == 0} {
                #puts "EnsurePopulated $tbl $row works"
                set r [$tbl insertchild $row end {row 1}]
                $tbl rowconfigure $r -name "c1"
                #
                set r [$tbl insertchild $row end {row 2}]
                $tbl rowconfigure $r -name "c2"
                #
                set r [$tbl insertchild $row end {another row}]
                $tbl rowconfigure $r -name "c3"
                #$tbl insertchildlist $row 0 {{row 1} {row 2} {another row}}
            }
        }
    }
    

    proc expandCmd {tbl row} {
        EnsurePopulated $tbl $row
    }

    proc TreeSetTo2 {tbl index} {
        $tbl selection anchor $index
        $tbl activate $index
        $tbl selection set $index $index
    }
    
    
    # Some actions are delayed to after idle
    proc TreeSetTo {tbl index} {
        $tbl selection clear 0 end
        $tbl see $index
        after idle "::srchtblst::TreeSetTo2 $tbl $index"
    }    

    proc MakeDefaultSearchState {tbl searchString} {
        return [dict create                                                       \
                    -continueP     0                                              \
                    -startFrom     [$tbl index anchor]                            \
                    -direction     "forwards"                                     \
                    -searchStringQ [QuoteStringForRegexp $searchString]           \
                    -findcase      0                                              \
                   ]
    }

    proc GetSearchStateIncrement {SearchState} {
        set direction [dict get $SearchState -direction]
        if {$direction eq "backwards"} {
            set increment -1
        } elseif {$direction eq "forwards"} {
            set increment 1
        } else {
            error "wrong direction $direction"
        }
        return $increment
    }

    proc SearchStateTableListCmdWithCaseOption {SearchState} {
        set findcase [dict get $SearchState -findcase]
        if {$findcase eq "1"} {
            set result "regexp"
        } elseif {$findcase eq "0"} {
            set result "regexp -nocase"
        } else {
            error "wrong findcase $findcase"
        }
        return $result
    }


    # Continuation for TreeSearchText
    proc TreeSearchTextC1 {CurName lambda tbl SearchState EnsurePopulatedCmd ContinuationBody} {
        set searchString [dict get $SearchState -searchStringQ]
        set CmdWithCaseOption [SearchStateTableListCmdWithCaseOption $SearchState]
        set celltext [$tbl get $CurName]
        set cmd [string cat $CmdWithCaseOption " " [list $searchString [lindex $celltext 0]]]
        # puts "cmd=$cmd"
        if {[eval $cmd]} {
            # found 
            ::srchtblst::TreeSetTo $tbl $CurName
            dict set SearchState -continueP 1
            dict set SearchState -startFrom $CurName
            apply $lambda $tbl 1 $SearchState
            return
        } else {
            set index [$tbl index $CurName]
            set increment [GetSearchStateIncrement $SearchState]
            set i [expr $index + $increment ]
            after idle [list ::srchtblst::TreeSearchTextInner $i $tbl $SearchState $EnsurePopulatedCmd $ContinuationBody]
        }
    }

    # Interactive search in existing items of a tree
    # Async command, does not return
    # Tries to expand subtrees. To disable, pass ProcedureNop as EnsurePopulatedCmd
    # Args: tbl - tablelist widget
    # Feature: shows only once per cell
    # Applies continuation with arguments bound:
    # tablelist: widget
    # found: 1=found, 0=not found;
    # SearchState: new SearchState
    # Items must be named
    # !!!Searches in first column only!!!
    proc TreeSearchText {tbl SearchState EnsurePopulatedCmd ContinuationBody} {
        set startFrom [dict get $SearchState -startFrom]
        TreeSearchTextInner $startFrom $tbl $SearchState $EnsurePopulatedCmd $ContinuationBody
    }
        
    proc TreeSearchTextInner {startFrom tbl SearchState EnsurePopulatedCmd ContinuationBody} {
        set continueP [dict get $SearchState -continueP]
        set increment [GetSearchStateIncrement $SearchState]
        set lambda [list {tablelist found SearchState} $ContinuationBody]
        # Coerce key (which can be a name) to index
        set startFrom [$tbl index $startFrom]
        set i [expr $startFrom + $increment * $continueP]
        if {0 <= $i && $i < [$tbl index end]} {
            set CurName [$tbl rowcget $i -name]
            if {$EnsurePopulatedCmd ne {}} {
                eval [list $EnsurePopulatedCmd $tbl $CurName]
            }
            after idle [list ::srchtblst::TreeSearchTextC1 $CurName $lambda $tbl $SearchState $EnsurePopulatedCmd $ContinuationBody]
            return 
        }
        apply $lambda $tbl 0 $SearchState
        return
    }

    # Orphan code which reports that key is not found. Write TreeSearchOuter around? 
    #    tk_messageBox -parent $tbl -message "key not found: $searchString"

    proc MakeTestTableList {} {

        destroy .w

        update
        
        toplevel .w

        tablelist::tablelist .w.t -columns {0 "First Column" 0 "Another column"} -stretch all -background white -stretch all -expandcommand ::srchtblst::expandCmd
        .w.t resetsortinfo

        pack .w.t -fill both -expand yes

        set data "tree1"
        set row [.w.t insertchild root end $data]
        .w.t rowconfigure $row -name "a"
        
        .w.t collapse $row

        set data "---"
        set row [.w.t insertchild root end $data]
        .w.t rowconfigure $row -name "aa"
        

        set data "tree0"
        set row [.w.t insertchild root end $data]
        .w.t rowconfigure $row -name "b"
        
        wm deiconify .w
    }

    proc TableListTest1 {} {
        MakeTestTableList
        set state [MakeDefaultSearchState .w.t "t"]
        dict set state -findcase 1
        ::srchtblst::TreeSearchText .w.t $state ::srchtblst::EnsurePopulated {
            puts "SearchState = $SearchState"
            if {!$found} {error "TableListTest1 failure 1"} else {puts "found 1"}
            ::srchtblst::TreeSearchText .w.t $SearchState ::srchtblst::EnsurePopulated {
                if {!$found} {error "TableListTest1 failure 2"} else {puts "found 2"}
                # after idle after idle destroy .w
            }
        }
    }

    # Opens a FindBox. You can work with it
    proc TableListExample {} {
        MakeTestTableList
        ::fndrpl::OpenFindBox .w.t "tablelist" "find" ::srchtblst::EnsurePopulated 
    }
}
