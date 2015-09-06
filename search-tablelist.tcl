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


    # Interactive search in existing items of a tree
    # Does not try to expand subtrees (is this a TODO?)
    # Args: tbl - tablelist widget
    # Feature: shows only once per cell
    # Returns list of two values: 1. 0-found,1-not found; 2-new SearchState            
    proc TreeSearchText {tbl SearchState EnsurePopulatedCmd} {
        set continueP [dict get $SearchState -continueP]
        set startFrom [dict get $SearchState -startFrom]
        set searchString [dict get $SearchState -searchStringQ]
        set increment [GetSearchStateIncrement $SearchState]
        set CmdWithCaseOption [SearchStateTableListCmdWithCaseOption $SearchState]
        for {set i [expr $startFrom + $increment * $continueP]} \
            {0 <= $i && $i <= [$tbl index end]} \
            {incr i $increment} {
                if {$EnsurePopulatedCmd ne {}} {
                    puts "EnsurePopulatedCmd = $EnsurePopulatedCmd"
                    eval [list $EnsurePopulatedCmd $tbl $i]
                }
                set celltext [$tbl get $i]
                set cmd [string cat $CmdWithCaseOption " " [list $searchString [lindex $celltext 0]]]
                # puts "cmd=$cmd"
                if {[eval $cmd]} {
                    after idle ::srchtblst::TreeSetTo $tbl $i
                    dict set SearchState -continueP 1
                    dict set SearchState -startFrom $i
                    return [list 1 $SearchState]
                }
            }
        return [list 0 $SearchState]
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

        set data "tree0"
        set row [.w.t insertchild root end $data]
        .w.t rowconfigure $row -name "b"
        
        wm deiconify .w
    }

    proc TableListExample {} {
        MakeTestTableList
        set state [MakeDefaultSearchState .w.t "t"]
        dict set state -findcase 1
        foreach {result state} [TreeSearchText \
                                    .w.t $state \
                                    ::srchtblst::EnsurePopulated] {break}
        tk_messageBox -message $state
        TreeSearchText .w.t $state ::srchtblst::EnsurePopulated
    }
}
