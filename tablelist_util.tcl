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

    proc TreeSetTo2 {tbl index} {
        $tbl selection anchor $index
        $tbl activate $index
        $tbl selection set $index $index
        #putd "Leaving TreeSetTo2"
    }
    
    
    # Some actions are delayed to after idle
    proc TreeSetTo {tbl index} {
        #putd "Entering TreeSetTo"
        $tbl selection clear 0 end
        $tbl see $index
        #after idle "::srchtblst::TreeSetTo2 $tbl $index"
        TreeSetTo2 $tbl $index
    }    
}
    
