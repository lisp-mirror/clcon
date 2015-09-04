package require Tk
package require tablelist


proc EnsurePopulated {tbl row} {
    if {[$tbl childcount $row] == 0} {
        $tbl insertchild $row 0 {another row}
        $tbl insertchild $row 0 {row 2}
        $tbl insertchild $row 0 {row 1}
        #$tbl insertchildlist $row 0 {{row 1} {row 2} {another row}}
    }
}
        

proc expandCmd {tbl row} {
    EnsurePopulated $tbl $row
}

proc TreeSetTo {tbl index} {
    $tbl see $index
    $tbl selection clear 0 end
    $tbl selection anchor $index
    $tbl activate $index
    $tbl selection set $index $index
}    

proc MakeDefaultSearchState {tbl searchString} {
    return [dict create                                                       \
                -continueP 0                                                  \
                -startFrom [$tbl index anchor]                                \
                -direction "forwards"                                         \
                -searchStringQ [QuoteStringForRegexp $searchString]           \
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

# Interactive search in existing items of a tree
# Does not try to expand subtrees (is this a TODO?)
# Args: tbl - tablelist widget
# Feature: shows only once per cell
# package require tk
# TkconSourceHere utils.tcl
# SearchState is a [dict continueP {0|1} startFrom {}|key direction {forwards}|{backwards}]
# Returns list of two values: 1. 0-found,1-not found; 2-new SearchState            
proc TreeSearchText {tbl SearchState} {
    set continueP [dict get $SearchState -continueP]
    set startFrom [dict get $SearchState -startFrom]
    set searchString [dict get $SearchState -searchStringQ]
    set increment [GetSearchStateIncrement $SearchState]
    for {set i [expr $startFrom + $increment * $continueP]} \
        {0 <= $i && $i <= [$tbl index end]} \
        {incr i $increment} {
            set celltext [$tbl get $i]
            if {[regexp -nocase $searchString [lindex $celltext 0]]} {
                after idle TreeSetTo .w.t $i
                dict set SearchState -continueP 1
                dict set SearchState -startFrom $i
                return [list 1 $SearchState]
            }
        }
    return [list 0 $SearchState]
}

# Orphan code which reports that key is not found. Write TreeSearchOuter around? 
#    tk_messageBox -parent $tbl -message "key not found: $searchString"


toplevel .w

tablelist::tablelist .w.t -columns {0 "First Column" 0 "Another column"} -stretch all -background white -stretch all -expandcommand expandCmd
.w.t resetsortinfo


pack .w.t -fill both -expand yes
set data "tree1"
.w.t insertchildlist root end $data
.w.t collapse end
# .w.t expand end
set data "tree0"
.w.t insertchildlist root end $data

wm deiconify .w
