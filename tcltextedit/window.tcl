namespace eval win {

    namespace export activate updateln Next Prev pton update status_change count exit names

    proc names {} {
        global window numw
        set r ""

        for {set i 1} {$i < $numw+1} {incr i} {
            if {$window($i,echange)==1} {
                set r "$r $i"
            }
        }
        return $r
    }

    proc realnames {} {
        global window numw
        set r ""

        for {set i 1} {$i < $numw+1} {incr i} {
            if {$window($i,echange)==1} {
                set r "$r [pton $window($i,name)]"
            }
        }
        return $r
    }

    proc exit {} {
        global c
        c
        set gr [wm grid .]
        set WW [expr (([winfo width .]) / [lindex $gr 2]) -3 ]
        set HH [expr (([winfo height .]) / [lindex $gr 3]) -5 ]
        set q "x"
        set c(geometry) "=$WW$q$HH"
    }

    proc count {} {
        global numw window
        set num 0
        for {set i 1} {$i < $numw+1 } {incr i} { 
            if {$window($i,echange)==1} {incr num}
        }
        return $num
    }

    proc activate { n } {
        global lw current_window numw env window home c undo_active
        c

        set undo_active 0

        if {$n!=-1} {
            if {$lw!=$n} {

                if {$window($n,temp)==""} {assigntempfile $n}
                if {$window($current_window,temp)==""} {assigntempfile $current_window}

                set current_window $n

                #swap out the old file only if it's not the clipboard
                switch $lw {
                    100 	{
                        clipboard clear -displayof .text
                        clipboard append -displayof .text  [.text get 0.1 "end-1 char"]
                        .menu.file entryconfigure [.menu.file index "Save"] -state normal 
                    }

                    200    {
                        .menu.file entryconfigure [.menu.file index "Save"] -state normal 
                    }

                    300    {
                        .menu.file entryconfigure [.menu.file index "Save"] -state normal 
                        .text configure -background $c(color-editbg) -state normal
                    }

                    default { 
                        if {$window($lw,echange)==1} {
                            file::DoSaveFile "file $window($lw,temp)" 
                        } else {
                            c "Window $lw was removed, deleting tmp file $window($lw,temp) "
                            if {$window($lw,temp)!=""} { file delete $window($lw,temp) -force }     
                            #This is also done in file::CloseFile
                            set window($lw,temp) ""
                        }
                    }
                }


                #swap in the new file only if it's not the clipboard
                switch $n {
                    100   {
                        .text delete 1.0 end
                        tk_textPaste .text
                        .menu.file entryconfigure [.menu.file index "Save"] -state disabled
                        win::update
                        .menu.windowmenu entryconfigure [.menu.windowmenu index "View/Edit Clipboard"] -state disabled
                    }                                              
                    200    {
                        UpdateFileList
                        .menu.file entryconfigure [.menu.file index "Save"] -state disabled
                        win::update
                        .menu.windowmenu entryconfigure [.menu.windowmenu index "Filelist"] -state disabled
                    }
                    300    {
                        .menu.file entryconfigure [.menu.file index "Save"] -state disabled
                        pml::inserttext .text [pml::gettopic]
                        win::update
                        .menu.windowmenu entryconfigure [.menu.windowmenu index "Help"] -state disabled
                        .text configure -state disabled
                    }

                    default { 
                        file::DoLoadFile "file $window($current_window,temp)" 
                        win::update
                    }
                }


                .text see $window($current_window,pos)
                tkTextSetCursor .text $window($current_window,pos)

                #set last window
                set lw $n
                win::status_change
            }
        }
        set undo_active 1
    }

    ### Proc to update line number in status line
    proc updateln {} {
        global window current_window
        set n [.text index insert]
        set window($current_window,pos) $n
    }


    proc Next {} {
        global current_window numw window
        set r 0
        c

        if {[win::count]==0} { 
            c "No windows left creating a new one"
            file::NewFile 
            return 0
        } else {

            set n $current_window

            if {$n==100} {set n 1}
            if {$n==200} {set n 1}
            if {$n==300} {set n 1}

            incr n
            while {$window($n,echange)==0} {
                incr n
                if {$n>100} {
                    set n 1
                    incr r
                } 
                if {$r>3} {
                    set n 1
                    c "Break!!"
                    break
                }
            }

            set current_window $n
            win::activate $current_window
            return 1
        }
    }

    proc Prev {} {
        global current_window numw window
        c

        if {[win::count]==0} { 
            c "No windows left creating a new one"
            file::NewFile 
        } else {

            set n $current_window

            if {$n==200} {set n 100}
            if {$n==300} {set n 100}

            set n [expr $n -1 ]
            if {$n<1} {set n 100}

            while {$window($n,echange)==0} {
                set n [expr $n -1 ]
                if {$n<1} {set n 100}
            }

            set current_window $n
            win::activate $current_window
        }
    }


    proc pton {path} {
        set n [file split $path]
        set n [lindex $n [expr [llength $n]-1]]
        return $n
    }


    proc update {} {
        global prgname current_window numw c window
        c

        wm title      . "$prgname - [pton $window($current_window,name)]"


        .menu.windowmenu delete 0 [expr $numw+1]
        for {set i 1} {$i < $numw+1 } {incr i} {

            if {$window($i,echange)==1} {
                .menu.windowmenu add command -label "$i. [pton $window($i,name)]"  -command "win::activate $i"
            }

        }
        .menu.windowmenu add separator
        .menu.windowmenu add command -label "Filelist" -underline 5 -command "win::activate 200"
        .menu.windowmenu add command -label "Help" -underline 0 -command "win::activate 300"
        .menu.windowmenu add command -label "View/Edit Clipboard" -underline 5 -command "win::activate 100"



        #Make the current window disabled in the menu
        set n [lsearch [win::names] $current_window]

        if {$n!=-1} {
            .menu.windowmenu entryconfigure $n -state disabled
        }

    }

    proc status_change {args} {
        global window current_window c prgname

        .status.l2 configure -text "Filename: $window($current_window,name) "

        #if {$window($current_window,change)==0} {}
        if { [.text edit modified] == 0 } {
            .status.l1 configure -foreground $c(color-statustext) -text  "Unchanged"
        } else {
            .status.l1 configure -foreground $c(color-statustextchanged) -text "Changed"
        }

        .status.l3 configure -text "Line: $window($current_window,pos)"
        .status.l2 configure -text "Filename: $window($current_window,name) "

    }

}

