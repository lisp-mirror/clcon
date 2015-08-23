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

    set reply [InitInspector $LispExpr]

    # We well parse data here.

    putd "reply = $reply"
    set ReplyAsDict [::mprs::Unleash $reply]
    set InspectedTitle [dict get $ReplyAsDict :title]
    set InspectedContentU [::mprs::Unleash [dict get $ReplyAsDict :content]]
    set InspectedData [::mprs::Unleash [lindex $InspectedContentU 0]]
    set InspectedMagicNumbers [lindex $InspectedContentU 1 end]

    putd "InspectedData = $InspectedData"
    
    #set reply "A proper list: \
#0: a \
#1: 2 "
    
    set w [PrepareGui1]

    $w.title.text insert 0.0 "$InspectedTitle\nMagic numbers: $InspectedMagicNumbers"

    set b $w.body.text

    foreach s $InspectedData {
        if {[::mprs::Consp $s] == 1} {
            set item [::mprs::Unleash $s]
            if {[lindex $item 0] eq :value} {
                ::tkcon::WriteActiveText $b [::mprs::Unleash [lindex $item 1]] end "tk_messageBox -message [lindex $item 2] -parent $w"
            } else {
                $b insert end "I don't know what is $s"
            }
        } else {
            $b insert end [::mprs::Unleash $s]
        }
    }

    
#    $w.body.text insert 0.0 $InspectedData

#    ::tkcon::WriteActiveText $w.body.text "blabla" end {tk_messageBox -message "ура!"}

    PrepareGui2 $w
}


# Make toplevel widget and its children 
proc ::insp::PrepareGui1 {} {
    variable ::tkcon::PRIV
    # Create unique edit window toplevel
    set w $PRIV(base).__edit
    set i 0
    while {[winfo exists $w[incr i]]} {}
    append w $i
    toplevel $w
    wm withdraw $w

    # title 
    set word "Инспектор"
    if {[string length $word] > 20} {
	wm title $w "[string range $word 0 16]... - tkcon Edit"
    } else {
	wm title $w "$word - tkcon Edit"
    }

# --------------------------------- frames-----------------              
    
# making title frame
    frame $w.title
    # height 2 - for magic numbers
    text $w.title.text -height 2
    scrollbar $w.title.sx -orient h -command [list $w.title.text xview]
    scrollbar $w.title.sy -orient v -command [list $w.title.text yview]
    ConfigureTextFonts $w.title.text
    $w.title.text configure \
        -xscrollcommand [list $w.title.sx set] \
        -yscrollcommand [list $w.title.sy set] 
        
# make body frame    
    frame $w.body
    text $w.body.text

    ConfigureTextFonts $w.body.text
    $w.body.text configure \
	-xscrollcommand [list $w.body.sx set] \
	-yscrollcommand [list $w.body.sy set] 
    catch {
	# 8.5+ stuff
	set tabsp [expr {$OPT(tabspace) * [font measure $OPT(font) 0]}]
	$w.body.text configure -tabs [list $tabsp left] -tabstyle wordprocessor
    }

    scrollbar $w.body.sx -orient h -command [list $w.body.text xview]
    scrollbar $w.body.sy -orient v -command [list $w.body.text yview]

# ----------------------------------- menu -------------------
    
    set menu [menu $w.mbar]
    $w configure -menu $menu
    
    FileMenu $w $menu $w.body.text
    EditMenu $w $menu $w.body.text

    return $w
}


# layout and show inspector window
proc PrepareGui2 {w} {    
# --------------------------------- pack ---------------------
# layout body elements in body 
    grid $w.body.text - $w.body.sy -sticky news
    grid $w.body.sx - -sticky ew
    grid columnconfigure $w.body 0 -weight 1 
    grid columnconfigure $w.body 1 -weight 1
    grid rowconfigure $w.body 0 -weight 1 

# now layout title elements in title
    grid $w.title.text - $w.title.sy -sticky news
    grid $w.title.sx - -sticky ew
    grid columnconfigure $w.title 0 -weight 1
    grid columnconfigure $w.title 1 -weight 1
    grid rowconfigure $w.title 0 -weight 1

    # combine the entire widget
    pack $w.title -side top -fill x 
    pack $w.body -fill both
    
    wm deiconify $w
    focus $w.body.text
}


proc ::insp::FileMenu {w menu text} {
    set m [menu [::tkcon::MenuButton $menu File file]]
    $m add command -label "Save As..."  -underline 0 \
	-command [list ::tkcon::Save {} widget $text]
    $m add command -label "Append To..."  -underline 0 \
	-command [list ::tkcon::Save {} widget $text a+]
    $m add separator
    $m add command -label "Dismiss" -underline 0 -accel "Control-w" \
	-command [list destroy $w]
    bind $w <Control-Key-w>		[list destroy $w]
    bind $w <Control-Key-Cyrillic_tse>		[list destroy $w]
}

proc ::insp::EditMenu {w menu text} {
    set m [menu [::tkcon::MenuButton $menu Edit edit]]
    $m add command -label "Copy"  -under 0 \
	-command [list tk_textCopy $text]
    $m add separator

    $m add command -label "Find" -under 0 \
	-command [list ::tkcon::FindBox $text]
    bind $w <Control-Key-f>             [list ::tkcon::Findbox $text]
    bind $w <Control-Key-Cyrillic_a>             [list ::tkcon::Findbox $text]
}    
