# An attempt of generic Findbox facility
# Based on ::tkcon::FindBox

## Copyright (c) 1995-2011 Jeffrey Hobbs, jeff(a)hobbs(.)org
## source standard_disclaimer.tcl
## source bourbon_ware.tcl
## Copyright (c) Denis Budyak 2015

## ::tkcon::FindBox - creates minimal dialog interface to ::tkcon::Find
# ARGS:	w	- text widget
#	str	- optional seed string for ::tkcon::PRIV(find)
##
proc ::tkcon::FindBox {w {str {}}} {
    variable PRIV

    set base $PRIV(base).find
    if {![winfo exists $base]} {
	toplevel $base
	wm withdraw $base
	catch {wm attributes $base -type dialog}
	wm title $base "tkcon Find"
	wm resizable $base 1 0

	label $base.l -text "Find:" -anchor e
	entry $base.e -textvariable ::tkcon::PRIV(find)

	checkbutton $base.case -text "Case Sensitive" \
	    -variable ::tkcon::PRIV(find,case)
	checkbutton $base.re -text "Use Regexp" \
	    -variable ::tkcon::PRIV(find,reg)

	frame $base.sep -borderwidth 1 -relief sunken -height 2
	frame $base.btn
	grid $base.l $base.e - - -sticky ew
	grid $base.case - $base.re -sticky ew
	grid $base.sep -columnspan 4 -sticky ew
	grid $base.btn -columnspan 4 -sticky ew
	grid columnconfigure $base 3 -weight 1

	button $base.btn.fnd -text "Find" -width 6
	button $base.btn.clr -text "Clear" -width 6
	button $base.btn.dis -text "Dismiss" -width 6
	eval grid [winfo children $base.btn] -padx 4 -pady 2 -sticky ew
	if {$PRIV(AQUA)} { # corner resize control space
	    grid columnconfigure $base.btn \
		[lindex [grid size $base.btn] 0] -minsize 16
	}

	focus $base.e

	bind $base.e <Return> [list $base.btn.fnd invoke]
	bind $base.e <Escape> [list $base.btn.dis invoke]
    }
    $base.btn.fnd config -command "::tkcon::Find [list $w] \$::tkcon::PRIV(find) \
	    -case \$::tkcon::PRIV(find,case) -reg \$::tkcon::PRIV(find,reg)"
    $base.btn.clr config -command "
    [list $w] tag remove find 1.0 end
    set ::tkcon::PRIV(find) {}
    "
    $base.btn.dis config -command "
    [list $w] tag remove find 1.0 end
    wm withdraw [list $base]
    "
    if {$str ne ""} {
	set PRIV(find) $str
	$base.btn.fnd invoke
    }

    if {[wm state $base] ne "normal"} {
	wm deiconify $base
    } else { raise $base }
    $base.e select range 0 end
}

## ::tkcon::Find - searches in text widget $w for $str and highlights it
## If $str is empty, it just deletes any highlighting
# ARGS: w	- text widget
#	str	- string to search for
#	-case	TCL_BOOLEAN	whether to be case sensitive	DEFAULT: 0
#	-regexp	TCL_BOOLEAN	whether to use $str as pattern	DEFAULT: 0
##
proc ::tkcon::Find {w str args} {
    $w tag remove find 1.0 end
    set truth {^(1|yes|true|on)$}
    set opts  {}
    foreach {key val} $args {
	switch -glob -- $key {
	    -c* { if {[regexp -nocase $truth $val]} { set case 1 } }
	    -r* { if {[regexp -nocase $truth $val]} { lappend opts -regexp } }
	    default { return -code error "Unknown option $key" }
	}
    }
    if {![info exists case]} { lappend opts -nocase }
    if {$str eq ""} { return }
    $w mark set findmark 1.0
    while {[set ix [eval $w search $opts -count numc -- \
			[list $str] findmark end]] ne ""} {
	$w tag add find $ix ${ix}+${numc}c
	$w mark set findmark ${ix}+1c
    }
    $w tag configure find -background $::tkcon::COLOR(blink)
    catch {$w see find.first}
    return [expr {[llength [$w tag ranges find]]/2}]
}

