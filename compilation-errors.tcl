# Compilation error browser. Do we need a command for it (to see last compilation errors)?

# (:return
#  (:ok
#   (:compilation-result
#    ((:message "The function was called with one argument, but wants exactly zero." :severity :warning :location
#               (:location ... ... nil)
#               :references nil)
#     (:message "undefined variable: xxx" :severity :warning :location
#               (:location ... ... nil)
#               :references nil :source-context "--> PROGN SB-IMPL::%DEFUN MULTIPLE-VALUE-PROG1 PROGN \n==>\n  (BLOCK CLCO::BAR CLCO::XXX (CLCO::BAR 75))\n"))
#    nil 0.009999999776482582 t "/s2/clcon/err.fasl"))
#  15)

namespace eval ::coer {
}

# Input - Leashed 
proc ::coer::ShowCompilationErrors { CompRes } {
}

# use ::insp::ConfigureTextFonts
# ::insp::ParseReturnOk might be useful for us.

proc ::coer::InsertDataToShowOrBeep { w CompResFromLisp } {
    # Put evaluation code from CompileLispFileTmp
    # to a separate place, so that we could have standard sequence for the situation
    # where lisp prepares code for tcl. Take care about querying user about loading failed
    # compilation (or do we simplify it somehow?)
    # We well parse data here.
    
    # bind var for convenience
    set b [BodyTextOfInspector $w]

    # clear old data if it existed
    [TitleOfInspector $w] delete 0.0 end
    $b delete 0.0 end
    
    # put this code to lisp side or not? I think yes. 
    [TitleOfInspector $w] insert 0.0 "$InspectedTitle\nMagic numbers: $InspectedMagicNumbers"
}

# This is a contiuation assigned on reply on initialization request 
proc ::coer::SwankInspect1 { EventAsList } {
    set w [PrepareGui1]
    InsertDataToShowOrBeep $w $EventAsList
    PrepareGui2 $w
}

# Note this. This can be useful
#proc ::coer::InspectorReinspect { w } {
#    set ContId [::tkcon::GenContinuationCounter]
#    set OnReply "::coer::ShowSomethingNewInInspector $w \$EventAsList"
#    ::tkcon::EvalInSwankAsync "(swank:inspector-reinspect)" $OnReply 0 t $ContId
#}


# Make toplevel widget and its children 
proc ::coer::PrepareGui1 {} {
    variable ::tkcon::PRIV
    # Create unique edit window toplevel
    set w $PRIV(base).__inspector
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
    InspectMenu $w $menu $w.body.text

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

proc ::coer::FileMenu {w menu text} {
    set m [menu [::tkcon::MenuButton $menu "1.File" file]]
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

proc ::coer::EditMenu {w menu text} {
    set m [menu [::tkcon::MenuButton $menu "2.Edit" edit]]
    $m add command -label "Copy"  -under 0 \
	-command [list tk_textCopy $text]
    $m add separator

    $m add command -label "Find" -under 0 \
	-command [list ::tkcon::FindBox $text]
    bind $w <Control-Key-f>             [list ::tkcon::Findbox $text]
    bind $w <Control-Key-Cyrillic_a>             [list ::tkcon::Findbox $text]
}    


proc ::coer::InspectMenu {w menu text} {
    set m [menu [::tkcon::MenuButton $menu "3.Inspect" inspect]]

    $m add command -label "Back" -accelerator <BackSpace> -command [list ::coer::InspectorPop $w]
    bind $w <BackSpace> [list ::coer::InspectorPop $w]
    bind $w <Alt-Key-Left> [list ::coer::InspectorPop $w]

    $m add command -label "Forward" -accelerator <Alt-Key-Right> -command [list ::coer::InspectorNext $w]
    bind $w <Alt-Key-Right> [list ::coer::InspectorNext $w]

    $m add command -label "Refresh" -accelerator <F5> -command [list ::coer::InspectorReinspect $w]
    bind $w <F5> [list ::coer::InspectorReinspect $w]
    
}    


proc ::coer::TitleOfInspector {w} {
    return $w.title.text
}

proc ::coer::BodyTextOfInspector {w} {
    return $w.body.text
}

proc ::coer::DebugStartup {} {
    variable ::tkcon::OPT
    if { $OPT(putd-enabled) } {
        ::coer::InitInspector {'defun}
    }
}


#::coer::DebugStartup
