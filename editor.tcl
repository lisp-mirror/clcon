## edit - opens a file/proc/var for reading/editing
## 
# Arguments:
#   type	proc/file/var
#   what	the actual name of the item
# Returns:	nothing
# Added by budden : -offset option, accepts index
## 
proc edit {args} {
    variable ::tkcon::PRIV
    variable ::tkcon::COLOR
    variable ::tkcon::OPT

    array set opts {-find {} -type {} -attach {} -wrap {none} -offset {}}
    while {[string match -* [lindex $args 0]]} {
	switch -glob -- [lindex $args 0] {
	    -f*	{ set opts(-find) [lindex $args 1] }
	    -a*	{ set opts(-attach) [lindex $args 1] }
	    -t*	{ set opts(-type) [lindex $args 1] }
	    -w*	{ set opts(-wrap) [lindex $args 1] }
            -o* { set opts(-offset) [lindex $args 1] }
	    #--	{ set args [lreplace $args 0 0]; break }
            --	{ break }
	    default {return -code error "unknown option \"[lindex $args 0]\""}
	}
	set args [lreplace $args 0 1]
    }
    # determine who we are dealing with
    if {[llength $opts(-attach)]} {
	foreach {app type} $opts(-attach) {break}
    } else {
	foreach {app type} [tkcon attach] {break}
    }

    set word [lindex $args 0]
    if {$opts(-type) == {}} {
	if {[llength [::tkcon::EvalOther $app $type info commands [list $word]]]} {
	    set opts(-type) "proc"
	} elseif {[llength [::tkcon::EvalOther $app $type info vars [list $word]]]} {
	    set opts(-type) "var"
	} elseif {[::tkcon::EvalOther $app $type file isfile [list $word]]} {
	    set opts(-type) "file"
	}
    }
    if {$opts(-type) == {}} {
	return -code error "unrecognized type '$word'"
    }

    # Create unique edit window toplevel
    set w $PRIV(base).__edit
    set i 0
    while {[winfo exists $w[incr i]]} {}
    append w $i
    toplevel $w
    wm withdraw $w
    if {[string length $word] > 20} {
	wm title $w "[string range $word 0 16]... - tkcon Edit"
    } else {
	wm title $w "$word - tkcon Edit"
    }

    set txt [text $w.text]
    $w.text configure -wrap $opts(-wrap) \
	-xscrollcommand [list $w.sx set] \
	-yscrollcommand [list $w.sy set] \
	-foreground $COLOR(stdin) \
	-background $COLOR(bg) \
	-insertbackground $COLOR(cursor) \
	-font $::tkcon::OPT(font) -borderwidth 1 -highlightthickness 0 \
	-undo 1
    catch {
	# 8.5+ stuff
	set tabsp [expr {$OPT(tabspace) * [font measure $OPT(font) 0]}]
	$w.text configure -tabs [list $tabsp left] -tabstyle wordprocessor
    }

    scrollbar $w.sx -orient h -command [list $w.text xview]
    scrollbar $w.sy -orient v -command [list $w.text yview]

    set menu [menu $w.mbar]
    $w configure -menu $menu

    ## File Menu
    ##
    set m [menu [::tkcon::MenuButton $menu File file]]
    $m add command -label "Save As..."  -underline 0 \
	-command [list ::tkcon::Save {} widget $w.text]
    $m add command -label "Append To..."  -underline 0 \
	-command [list ::tkcon::Save {} widget $w.text a+]
    $m add separator
    $m add command -label "Dismiss" -underline 0 -accel $PRIV(ACC)w \
	-command [list destroy $w]
    bind $w <$PRIV(CTRL)w>		[list destroy $w]
    bind $w <Alt-w>		[list destroy $w]

    ## Edit Menu
    ##
    set text $w.text
    set m [menu [::tkcon::MenuButton $menu Edit edit]]
    $m add command -label "Cut"   -under 2 \
	-command [list tk_textCut $text]
    $m add command -label "Copy"  -under 0 \
	-command [list tk_textCopy $text]
    $m add command -label "Paste" -under 0 \
	-command [list tk_textPaste $text]
    $m add separator
    $m add command -label "Find" -under 0 \
	-command [list ::tkcon::FindBox $text]

    ## Send To Menu
    ##
    set m [menu [::tkcon::MenuButton $menu "Send To..." send]]
    $m add command -label "Send To $app" -underline 0 \
	-command "::tkcon::EvalOther [list $app] $type \
		eval \[$w.text get 1.0 end-1c\]"
    set other [tkcon attach]
    if {[string compare $other [list $app $type]]} {
	$m add command -label "Send To [lindex $other 0]" \
	    -command "::tkcon::EvalOther $other \
		    eval \[$w.text get 1.0 end-1c\]"
    }

    grid $w.text - $w.sy -sticky news
    grid $w.sx - -sticky ew
    grid columnconfigure $w 0 -weight 1
    grid columnconfigure $w 1 -weight 1
    grid rowconfigure $w 0 -weight 1

    switch -glob -- $opts(-type) {
	proc*	{
	    $w.text insert 1.0 \
		    [::tkcon::EvalOther $app $type dump proc [list $word]]
	    after idle [::tkcon::Highlight $w.text tcl]
	}
	var*	{
	    $w.text insert 1.0 \
		    [::tkcon::EvalOther $app $type dump var [list $word]]
	    after idle [::tkcon::Highlight $w.text tcl]
	}
	file	{
	    $w.text insert 1.0 [::tkcon::EvalOther $app $type eval \
		    [subst -nocommands {
		set __tkcon(fid) [open {$word} r]
		set __tkcon(data) [read \$__tkcon(fid)]
		close \$__tkcon(fid)
		after 1000 unset __tkcon
		return \$__tkcon(data)
	    }
	    ]]
	    after idle [::tkcon::Highlight $w.text \
			    [string trimleft [file extension $word] .]]
	}
	error*	{
	    $w.text insert 1.0 [join $args \n]
	    after idle [::tkcon::Highlight $w.text error]
	}
	default	{
	    $w.text insert 1.0 [join $args \n]
	}
    }
    wm deiconify $w
    focus -force $w.text
    if {[string compare $opts(-find) {}]} {
	::tkcon::Find $w.text $opts(-find) -case 1
    }
    if {$opts(-offset) ne {}} {
	$w.text mark set insert $opts(-offset)
        $w.text see insert
    }
}
