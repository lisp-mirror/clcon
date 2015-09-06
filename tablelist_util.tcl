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
}
    
