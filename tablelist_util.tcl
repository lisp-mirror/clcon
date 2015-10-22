# tablelist general utilities
# (c) Denis Budyak
# include MIT License 
# package require tablelist

namespace eval ::tablelist_util {
    proc CopyCurrentCell {tbl} {
        clipboard clear
        # set index [$tbl cellindex active]
        set txt [$tbl cellcget active -text]
        clipboard append $txt
    }

    proc TreeSetTo {tbl index} {
        GotoIndex $tbl $index
    }


    # Sorting by clicking on column title. 
    proc ReverseSortOrder {oldSortOrder} {
        if { $oldSortOrder eq {increasing} } {
            return {-decreasing}
        } else {
            return {-increasing}
        }
    }       

    proc SortOrReSort {tablelist column} {
        set t $tablelist
        set oldSortColumn [$t sortcolumn]
        set oldSortOrder [$t sortorder]
        if {$oldSortColumn == $column} {
            set sortOrder [ReverseSortOrder $oldSortOrder]
        } else {
            set sortOrder {-increasing}
        }
        $t sortbycolumn $column $sortOrder
    }

    
    proc BindReSortingToClickingOnColumnLabel {tbl} {
        $tbl configure -labelcommand ::tablelist_util::SortOrReSort
    }

    proc GotoIndex {tbl index} {
        set ind [$tbl index $index]
        $tbl selection clear 0 end
        $tbl selection set $ind $ind
        $tbl activate $ind
        $tbl selection anchor $ind
        $tbl see $ind
    }
}
    
