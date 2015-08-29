#?-Speedbuttons
#?
#? %2Abstract:%
#? Speed buttons is the buttons just below the menu bar.
#? All speed buttons is linked to a user defined macro.
#?
#? %2Adding:%
#? To add a speed button "right-click" anywhere on the speed button bar and
#? select Assign, and a list with all available macros will appear.
#? Just select the macro you want to add and a button with that macros name
#? will apear.
#?
#? %2Remove:%
#? To remove a speed button "right-click" on the speed button you want to remov
#? and select remove this.
#?
#? Related topics: %lMACROS%
#?


namespace eval speed {

    namespace export Update Button Side Assign Remove Edit


    proc Edit {} {
        global current_button c home
        c $current_button

        macro::rec global current_button
        macro::rec "set current_button $current_button"
        macro::rec "speed::Edit"

        set n "file $home/[lindex $c(speed) [expr $current_button - 1]].mac"
        c "file $n"

        file::Load $n -force

        .mnu unpost
    }


    proc Remove {} {
        global current_button c
        c $current_button

        macro::rec global current_button
        macro::rec "set current_button $current_button"
        macro::rec "speed::Remove"

        if {$current_button!=-1} {
            set n [expr $current_button - 1]
            set c(speed) [ lreplace $c(speed) $n $n]
            speed::Update
        }
        .mnu unpost
    }

    proc Assign {name} {
        global current_button c
        c $current_button

        macro::rec global current_button
        macro::rec "set current_button $current_button"
        macro::rec "speed::Assign $name"

        if {$current_button==-1} {
            set c(speed) "$c(speed) {$name}"
        } else {
            set n [expr $current_button - 1]
            set c(speed) [ lreplace $c(speed) $n $n $name ]
        }

        speed::Update
        .mnu unpost
    }


    proc Update {} {
        global current_button c

        c
        set menu .mnu
        destroy $menu
        menu $menu -tearoff 0
        menu $menu.macros

        $menu add command -label "Never mind" -command "$menu unpost"
        $menu add command -label "Remove this." -command "speed::Remove"
        $menu add command -label "Edit this." -command "speed::Edit"
        $menu add cascade -label "Assign" -menu $menu.list
        xmenu $menu.list 

        $menu configure -tearoff $c(tearoff) -background $c(color-menubg) -foreground $c(color-menutxt) -activebackground $c(color-menuactive) -activeforeground $c(color-menuactivetext)

        foreach n [macro::names] {
            $menu.list add command -label $n -command "speed::Assign {$n}"
        }
        set current_button -1


        #Delete all buttons within frame .wl
        foreach n [winfo children .wl] {
            destroy $n
        }

        #Add all relevant buttons to frame
        set i 1
        foreach n $c(speed) {
            xbutton  .wl.t$i -text "$n" -command "macro::play {$n}"
            pack .wl.t$i -side left
            bind .wl.t$i <Button-3> "speed::Button $i"
            incr i
        }

        if {[llength $c(speed)]==0} {
            label .wl.t -text "Right click here to add macro buttons" 
            pack .wl.t
            bind .wl.t <Button-3> speed::Side
        }

    }

    proc Button {n} {
        global current_button
        set current_button $n
        .mnu post [winfo pointerx . ] [winfo pointery .]
    }

    proc Side {} {
        global current_button
        set current_button -1
        .mnu post [winfo pointerx . ] [winfo pointery .]
    }


}

#Initialize
speed::Update



