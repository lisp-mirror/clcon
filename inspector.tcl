## ::tkcon::SwankInspect - opens a text for inspecting an object
## 
# Arguments:
#   LispExpr    lisp expression
# Returns:	nothing
##


# (:emacs-rex
#  (swank:inspect-nth-part 1)
#  "COMMON-LISP-USER" t 33)
# (:return
#  (:ok
#   (:title "#<symbol {DAA40AF}>" :id 0 :content
#           (("Its name is" ": "
#             (:value "\"A\"" 1)
#             "\n" "It is unbound." "\n" "It has no function value." "\n" "It is " "internal" " to the package: "
#             (:value "COMMON-LISP-USER" 2)
#             " "
#             (:action "[export]" 0)
#             " "
#             (:action "[unintern]" 1)
#             "\n" "Property list" ": "
#             (:value "nil" 3)
#             ...)
#            21 0 500)))

namespace eval ::insp {
}

## ::insp::InitInspector :
# Example of dialog: 
# (:emacs-rex
#  (swank:init-inspector "*")
#  "COMMON-LISP-USER" :repl-thread 32)
# (:return
#  (:ok
#   (:title "#<cons {BD735A3}>" :id 0 :content
#           (("A proper list:" "\n" "0" ": "
#             (:value "a" 1)
#             "\n" "1" ": "
#             (:value "2" 2)
#             "\n")
#            10 0 500)))
#  32)
# We can't still eval in :repl-thread synchonously, so this is just a demo.
proc ::insp::InitInspector { LispExpr } {

# (clco::convert-object-to-tcl '(:title "#<cons {BD735A3}>" :id 0 :content (("A proper list:" "\n" "0" ": "
#              (:value "a" 1)
#              "\n" "1" ": "
#              (:value "2" 2)
#              "\n")
#             10 0 500)))
    
    set result "{l:title s#<cons\\ \\{BD735A3\\}> :id n0 :content {l{lsA\\ proper\\ list: sn s0 s:\\  {l:value sa n1 } sn s1 s:\\  {l:value s2 n2 } sn } n10 n0 n500 } } "
    return $result             
}


proc ::clconcmd::insp {arg} {
    tkcon main ::insp::SwankInspect $arg
}


proc ::insp::ConfigureTextFonts {text} {
    variable ::tkcon::COLOR
    $text configure \
        -foreground $COLOR(stdin) \
        -background $COLOR(bg) \
        -insertbackground $COLOR(cursor) \
        -font $::tkcon::OPT(font) -borderwidth 1 -highlightthickness 0 \
        -undo 1
}


proc ::insp::SwankInspect { LispExpr } {
    variable ::tkcon::PRIV
    variable ::tkcon::OPT

    set reply {InitInspector $LispExpr}

    # We have to parse data and build representation. Also we can do that on lisp side.
    # For now, let's assume we have text and we must only show it.

    set InspectedTitle "#<cons {DB735A3}>"
    set InspectedContents "A proper list: \
0: a \
1: 2 "

    # Create unique edit window toplevel
    set w $PRIV(base).__edit
    set i 0
    while {[winfo exists $w[incr i]]} {}
    append w $i
    toplevel $w
    wm withdraw $w

    # title 
    set word "Inspector"
    if {[string length $word] > 20} {
	wm title $w "[string range $word 0 16]... - tkcon Edit"
    } else {
	wm title $w "$word - tkcon Edit"
    }

    set txt [text $w.text]

    ConfigureTextFonts $w.text
    $w.text configure \
	-xscrollcommand [list $w.sx set] \
	-yscrollcommand [list $w.sy set] 
    catch {
	# 8.5+ stuff
	set tabsp [expr {$OPT(tabspace) * [font measure $OPT(font) 0]}]
	$w.text configure -tabs [list $tabsp left] -tabstyle wordprocessor
    }

    scrollbar $w.sx -orient h -command [list $w.text xview]
    scrollbar $w.sy -orient v -command [list $w.text yview]

    set menu [menu $w.mbar]
    $w configure -menu $menu

    FileMenu $w $menu
    EditMenu $w $menu

# buddens experiments :) 
    frame $w.title
    text $w.title.text
    scrollbar $w.title.sx -orient h -command [list $w.title.text xview]
    scrollbar $w.title.sy -orient v -command [list $w.title.text yview]
    ConfigureTextFonts $w.title.text
    $w.title.text configure \
        -xscrollcommand [list $w.title.sx set] \
        -yscrollcommand [list $w.title.sy set] 
        
    $w.title.text insert 0.0 $InspectedTitle

# end of buddens experiments

    
    grid $w.title.text - $w.title.sy -sticky news
    grid $w.title.sx - -sticky ew
    
    grid $w.title -row 0
    grid $w.text -row 1 
    # grid $w.sx - -sticky ew
    grid columnconfigure $w 0 -weight 1
    grid columnconfigure $w 1 -weight 1
    grid rowconfigure $w 0 -weight 1
    grid rowconfigure $w 1 -weight 100
    
    $w.text insert end -----------------\n
    $w.text insert end $InspectedContents

    wm deiconify $w
    focus $w.text
}


proc ::insp::FileMenu {w menu} {
    set m [menu [::tkcon::MenuButton $menu File file]]
    $m add command -label "Save As..."  -underline 0 \
	-command [list ::tkcon::Save {} widget $w.text]
    $m add command -label "Append To..."  -underline 0 \
	-command [list ::tkcon::Save {} widget $w.text a+]
    $m add separator
    $m add command -label "Dismiss" -underline 0 -accel "Control-w" \
	-command [list destroy $w]
    bind $w <Control-Key-w>		[list destroy $w]
    bind $w <Control-Key-Cyrillic_tse>		[list destroy $w]
}

proc ::insp::EditMenu {w menu} {
    set text $w.text
    set m [menu [::tkcon::MenuButton $menu Edit edit]]
    $m add command -label "Copy"  -under 0 \
	-command [list tk_textCopy $text]
    $m add separator

    $m add command -label "Find" -under 0 \
	-command [list ::tkcon::FindBox $text]
    bind $w <Control-Key-f>             [list ::tkcon::Findbox $text]
    bind $w <Control-Key-Cyrillic_a>             [list ::tkcon::Findbox $text]
}    
