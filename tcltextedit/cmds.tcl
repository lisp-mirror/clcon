#?-RecentFiles
#? 
#? In the file->Recent.. menu you will find the last 
#? 6 files you have accessed
#?
#? Just select one file and if it still exists it will be loaded into
#? a new window.
#?

if {$debug_messages==2} {
    catch {destroy .logger}
    toplevel .logger
    wm title .logger "Logg"
    text .logger.text 
    pack .logger.text
    wm protocol  .logger WM_DELETE_WINDOW { destroy . }
}

proc c {args} {
    global debug_messages

    if {$debug_messages!=0} {

        set result ""

	if {[info level]!=1} { 
            set result  "[info level -1] --> $args \n"

	} else { set result "Root --> $args"  }

	if {$debug_messages==2} {
            .logger.text insert end $result
            .logger.text see end
	} else { puts stdout $result}

    }
}

proc progress {v max} {
    set v [expr $v * 100]
    set max [expr $max*100 ]

    set pp [ expr $v * $max ]

    .status.filler config -text "($v/$max)"
    update
}

proc updaterecent {} {
    global c
    .menu.file.rcent delete 0 end
    for {set i 1} {$i <16} {incr i } {
        if {[info exist c(recent$i)]} {
            .menu.file.rcent add command -label [lindex $c(recent$i) 1] -command "file::Load {$c(recent$i)} -force"
        }
    }
}

proc addrecent {s} {
    global c

    set flag 1

    for {set i $c(numrecent)} {$i >0} {set i [expr $i-1]} {
        if { $c(recent$i) == $s } { set flag 0 }
    }

    if {$flag==1} {
        for {set i $c(numrecent)} {$i >1} {set i [expr $i-1]} {
            set c(recent$i) $c(recent[expr $i-1])
        }

        set c(recent1) $s
    }

    updaterecent
}


proc powin {w} {
    set y [winfo y .]
    #set x [expr [winfo x .] + ([winfo width .]/2)  - ($wid/2) ]
    set x [expr [winfo x .] + ([winfo width .]/2)  - (200) ]
    wm geometry $w "=+$x+$y" 
    c [winfo width $w]
}

proc assigntempfile {win} {
    global home  window
    set i 0
    while {[file exist "$home/.ttemp.$i"]} {
        incr i
    } 
    set file "$home/.ttemp.$i"
    set window($win,temp) $file
    set f [open $file "CREAT RDWR" ]
    close $f      
    c "Window $win --> tempfile # $i"
}


proc rmtempfiles {} {
    global window
    set n 1
    while {$n<=100} {
        if {$window($n,echange)!=0} { 
            c "Deleting old tempfile $window($n,temp)"
            file delete $window($n,temp) -force
        }
        incr n
    }
}


proc UpdateFileList {} {
    global window numw current_window c
    c 
    .text delete 1.0 end
    for {set i 1} {$i < $numw+1 } {incr i} {
        if {$window($i,echange)==1} {
            .text insert end "$i. $window($i,name)\n" 
            set start "$i.0"
            .text tag add "F$start" $start end
            .text tag configure "F$start" -foreground $c(color-filelist)
            .text tag bind "F$start"  <Button-1> "win::activate $i"
        }
    }
}


