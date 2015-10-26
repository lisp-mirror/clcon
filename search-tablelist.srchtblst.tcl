package require Tk
package require tablelist
# TkconSourceHere util.tcl
# ::fndrpl::SearchState is documented in findreplace.tcl:

namespace eval ::srchtblst {

    # Sample EnsurePopulated proc.
    # Args:
    # tbl - tablelist
    # row - row id
    # ContinuationBody - body of a parameterless function
    # function is called when population is finished
    proc ExampleEnsurePopulated {tbl row ContinuationBody} {
        set name [$tbl rowcget $row -name]
        if {$name eq "a"} {
            if {[$tbl childcount $row] == 0} {
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
        set lambda [list {} $ContinuationBody]
        apply $lambda
    }
    

    proc ExampleExpandCmd {tbl row} {
        ExampleEnsurePopulated $tbl $row ProcedureNop
    }

    proc MakeDefaultSearchState {tbl searchString} {
        variable ::fndrpl::SearchState
        set SearchState                                                        \
            [dict create                                                       \
                 -continueP     0                                              \
                 -startFrom     [$tbl index active]                            \
                 -direction     "forwards"                                     \
                 -searchStringQ [QuoteStringForRegexp $searchString]           \
                 -findcase      0                                              \
                ]
    }

    proc GetSearchStateIncrement {} {
        variable ::fndrpl::SearchState
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

    # Returns command, including search string, as a list
    proc SearchStateTableListCmdWithCaseOption {} {
        variable ::fndrpl::SearchState
        set findcase [dict get $SearchState -findcase]
        set searchStringQ [dict get $SearchState -searchStringQ]
        if {$findcase eq "1"} {
            set result [list string match [string cat "*" $searchStringQ "*"]]
        } elseif {$findcase eq "0"} {
            set result [list string match -nocase [string cat "*" $searchStringQ "*"]]
        } else {
            error "wrong findcase $findcase"
        }
        return $result
    }


    # Continuation for TreeSearchText
    # kind=1 - first call (from TreeSearchText)
    # kind=0 - subsequent calls (loop iterations)
    proc TreeSearchTextC1 {kind CurName lambda tbl EnsurePopulatedCmd ContinuationBody} {
        variable ::fndrpl::SearchState
        if {![winfo exists $tbl]} {
            return
        }
        if {$kind == 1} { # first loop iteration
            set i [$tbl index $CurName]
            putd "i=$i, end = [$tbl index end]"
        } else { # not the first loop iteration - will move to next/previous row
            # Coerce name to text
            set CurName [$tbl rowcget $CurName -name]
            if {$CurName eq {}} {
                error "unnamed row in a table"
            }
            set searchString [dict get $SearchState -searchStringQ]
            if {$searchString eq {}} {
                apply $lambda $tbl -1
            }            
            set CmdWithCaseOption [SearchStateTableListCmdWithCaseOption]
            # set celltext [$tbl get $CurName]
            # We separate cells with tabs to decrease possibility of occasional finding of the entire search string
            # We could describe it is a search feature though.
            set celltext [list [join [$tbl rowcget $CurName -text] \t]]
            set cmd [concat $CmdWithCaseOption $celltext]
            putd "about to test row at $CurName = [$tbl index $CurName]"
            showVarPutd cmd
            if {[eval $cmd]} {
                # found 
                ::tablelist_util::TreeSetTo $tbl $CurName
                dict set SearchState -continueP 1
                dict set SearchState -startFrom $CurName
                apply $lambda $tbl 1
                return
            } else {
                set index [$tbl index $CurName]
                set increment [GetSearchStateIncrement]
                set i [expr $index + $increment ]
                putd "incremented i by $increment to be $i"
            }
        }
        
        if {0 <= $i && $i < [$tbl index end]} {
            set CurName [$tbl rowcget $i -name]
            set ContinuationCall \
                [list "after" "idle" [list "::srchtblst::TreeSearchTextC1" 0 $CurName $lambda $tbl $EnsurePopulatedCmd $ContinuationBody]]
            if {$EnsurePopulatedCmd ne {}} {
                putd "About to EnsurePopulatedCmd $EnsurePopulatedCmd $CurName"
                set call [list $EnsurePopulatedCmd $tbl $CurName $ContinuationCall]
                # putd $call
                eval $call
            } else {
                eval $ContinuationCall
            }
        } else { # hence out of index range
            apply $lambda $tbl 0 
        }
        return
    }

    # Interactive search in existing items of a tree
    # Async command, does not return. Calls ContinuationBody instead
    # Tries to expand subtrees with EnsurePopulatedCmd, where EnsurePopulatedCmd
    # is like ExampleEnsurePopulated.
    # To disable populating, try passing {} as EnsurePopulatedCmd
    #
    # Args: tbl - tablelist widget
    # Feature: shows only once per cell
    # After Search is finished, applies ContinuationBody with arguments bound:
    # tablelist: widget
    # found: 1=found, 0=not found;
    # SearchState: new SearchState
    # Items must be named
    # !!!Searches in first column only!!!
    proc TreeSearchText {tbl EnsurePopulatedCmd ContinuationBody} {
        variable ::fndrpl::SearchState
        set startFrom [dict get $SearchState -startFrom]
        set continueP [dict get $SearchState -continueP]
        set increment [GetSearchStateIncrement]
        set lambda [list {tablelist found} $ContinuationBody]
        # Coerce key (which can be a name) to index
        set startFrom [$tbl index $startFrom]
        putd "TreeSearchTextInner: startFrom = $startFrom, continueP = $continueP"
        set i [expr $startFrom + $increment * $continueP]
        ::srchtblst::TreeSearchTextC1 1 $i $lambda $tbl $EnsurePopulatedCmd $ContinuationBody 
        return
    }

    # Orphan code which reports that key is not found. Write TreeSearchOuter around? 
    #    tk_messageBox -parent $tbl -message "key not found: $searchString"

    proc MakeTestTableList {} {

        destroy .w

        update
        
        toplevel .w

        tablelist::tablelist .w.t -columns {0 "First Column" 0 "Another column"} -stretch all -background white -stretch all -expandcommand ::srchtblst::ExampleExpandCmd
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

    # Must pring "found 1" and "found 2"
    proc TestFnMan1 {} {
        variable ::fndrpl::SearchState
        MakeTestTableList
        [MakeDefaultSearchState .w.t "t"]
        dict set state -findcase 1
        ::srchtblst::TreeSearchText .w.t ::srchtblst::ExampleEnsurePopulated {
            if {!$found} {error "TableListTest1 failure 1"} else {puts "found 1"}
            ::srchtblst::TreeSearchText .w.t ::srchtblst::ExampleEnsurePopulated {
                if {!$found} {error "TableListTest1 failure 2"} else {puts "found 2"}
                # after idle after idle destroy .w
            }
        }
    }

    # Opens a FindBox. You can work with it and see if it works fine
    proc TestFnMan2 {} {
        MakeTestTableList
        ::fndrpl::OpenFindBox .w.t "tablelist" "find" ::srchtblst::ExampleEnsurePopulated 
    }
}
