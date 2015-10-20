# Drawing of recent menu
namespace eval ::recent {

    proc RecentMenu {FileSubMenu} {                                     
        variable RecentFilesList
        variable ::tkcon::PRIV

        InitRecentFilesListIfDoesNotExist

        set s $FileSubMenu.recent
        $s delete 0 end
        set i 1
        foreach filename $RecentFilesList {
            if {![::edt::IsFileBeingEdited $filename]} {
                # Here we must check if the file is not opened already
                set label [string cat $i $filename]
                $s add command -label $label -command [list ::edt::edit -type file -wrap char -- $filename] -under 0
                incr i
                if {$i > 9} {
                    # can't accelerate efficiently
                    break
                }
            }
        }
    }
}

