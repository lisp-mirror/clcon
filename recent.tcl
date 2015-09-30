# Array of recent values. This file must be loaded before loading or exiting code
# So that loading/saving recent history is properly evaluated

namespace eval ::recent {
    variable RecentFilesList
    # total amount of items remembered
    variable maxnumrecent 20
    # maximum capacity of recent menu
    variable RecentMenuCapacity 6

    # Init recent if it does not exist
    proc InitRecentFilesListIfDoesNotExist {} {
        variable RecentFilesList
        if {![info exist RecentFilesList]} {
            set RecentFilesList {}
        }
    }

    proc AddRecent {s} {
        variable RecentFilesList
        variable maxnumrecent

        InitRecentFilesListIfDoesNotExist

        set WhereFound [lsearch -exact $RecentFilesList $s]
        if {$WhereFound >= 0} {
            # MRU
            set RecentFilesList [concat         \
                                     [list $s]  \
                                     [lreplace $RecentFilesList $WhereFound $WhereFound]]
        } else {
            set RecentFilesList [concat         \
                                     [list $s]  \
                                     [lrange $RecentFilesList 0 [expr {$maxnumrecent - 1}]]]
        }
        
        RedrawRecentMenuForConsole
    }

    proc SaveRecentFilesList {fid} {
        variable RecentFilesList
        InitRecentFilesListIfDoesNotExist
        puts $fid [dump variable RecentFilesList]
    }
  
}
