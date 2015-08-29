#?-Mouse
#?
#? %2Popup menus:%
#? Push right button anywhere in the text box and the "Edit" menu will appear.
#? Push right button + shift and the Search menu will appear.
#?
#? %2Mark text%
#? Push the left button at the begining of the text you want to mark, move
#? the cursor to the end of the text you want to mark and release the button.
#?
#? Doubble clicking in the text will mark a whole word.
#? Tripple clicking in the text will mark a whole line.
#?
#? %2Wheel mouse ?%
#?
#? TCltextEdit is prepared for wheel mouse but it must be enabled in X
#?
#? This is how I did it, but I cant garantee that it will work for you.
#? I have a Logitech wheelmouse, and I use XFree86 Version 3.3.2.3.
#? 
#? This goes in to XF86Config under Section "Pointer"
#?
#?    Protocol    "intellimouse"
#?    Device      "/dev/mouse"
#?    ZAxisMapping 4 5
#?
#? Do not fiddle with these parameters if you're not sure of what you're doing.
#?
#?-StatusLine
#?
#? This is the status line, it is located just beneith the "text window"
#?
#? #1 Changed or Unchanged, Tells you if your text is saved or not.
#? #2 Indicates which line the cursor is positioned at, [row.column].
#? #3 The full name of the file in the active window.
#? #4 Status text, tells you important information about the editor.
#? #5 Push the left or right mouse button and rezise the window.
#?
#? %Pstatus.gif%
#?

#--------------------- Widgets ----------------------------
proc xbutton {w args} {
    c
    eval "button $w $args -borderwidth 1 -foreground black"
}

proc xentry {w args} {
    global c
    c
    eval "entry $w $args -background $c(color-editbg) -foreground $c(color-edittxt) -borderwidth 1"
}

proc xmenu {w args} {
    global c
    c
    eval "menu $w $args -tearoff $c(tearoff) -background $c(color-menubg) -foreground $c(color-menutxt) -activebackground $c(color-menuactive) -activeforeground $c(color-menuactivetext)"
}

#----------------------------------------------------------

proc gbn {n} {
    global c
    return "([lindex [split $c($n) "|"] 0])"
}

set dbutton 0

#make window buttons

frame .wl -height 20 -relief sunken -borderwidth 1 -background $c(color-background)
bind .wl <Button-3> speed::Side

frame .wb -background $c(color-menubg)

Supertext::text .text -yscrollcommand ".scrolly set"  -xscrollcommand ".scrollx set" \
    -setgrid true -wrap none -background $c(color-editbg) -foreground $c(color-edittxt) \
    -exportselection 0 -borderwidth 2 -highlightthickness 0  -insertbackground $c(color-cursor) \
    -font $c(font-editor) -borderwidth 1

scrollbar .scrolly -command ".text yview" -background $c(color-menubg) -activebackground $c(color-menuactive) -troughcolor $c(color-menubg)
scrollbar .scrollx -command ".text xview" -orient horiz -background $c(color-menubg) -activebackground $c(color-menuactive) -troughcolor $c(color-menubg)

frame .status -borderwidth 1 -relief flat 
label .status.l1 -text "1"  -relief sunken -borderwidth 1 -width 10
label .status.l2 -text "2" -relief sunken -borderwidth 1 
label .status.filler -text "" -relief sunken -borderwidth 1
label .status.l3 -text "3" -relief sunken -borderwidth 1 -width 13 

canvas .status.c -relief sunken -borderwidth 0 -width 20 -height 16 -cursor bottom_right_corner

pack .status.l1 .status.l3 .status.l2 -side left -padx 1
pack .status.filler -side left -fill x -expand yes -padx 1
pack .status.c  -padx 1

pack .status  -side bottom -fill x

.status.c create line 19 0   1 18 -fill white
.status.c create line 19 1   2 18 -fill gray70
.status.c create line 19 2   3 18 -fill gray70
.status.c create line 19 6   7 18 -fill white
.status.c create line 19 7   8 18 -fill gray70
.status.c create line 19 8   9 18 -fill gray70
.status.c create line 19 12   13 18 -fill white
.status.c create line 19 13   14 18 -fill gray70

bind .status.c <Motion> {
    global dbutton dxx dyy

    set gr [wm grid .]

    if {$dbutton} {
        set WW [expr ((%X-[winfo rootx .]) / [lindex $gr 2])-2 ]
        set HH [expr ((%Y-[winfo rooty .]) / [lindex $gr 3])-3 ]
        set q "x"
        if {$WW<56} { set WW 56 }
        if {$HH<1} { set HH 1 }
        wm geometry . "=$WW$q$HH"
        update
    }
}

bind .status.c <ButtonPress> {
    global dbutton dxx dyy
    set dxx %x
    set dyy %y
    set dbutton 1
}

bind .status.c <ButtonRelease> {
    global dbutton
    set dbutton 0
}
bind .status.c <FocusOut> {
    global dbutton
    set dbutton 0
}

button .b -text "check" -command {
    c [winfo rootx .]
    WsetWH . 100 100
}

pack .wl -fill x
pack .wb -fill x
pack .scrolly -side right -fill y
pack .scrollx -side bottom -fill x

pack .text  -expand yes -fill both -side left 


#------------------------------- Make menu --------------------------------
#menu .menu -borderwidth 0
xmenu .menu -borderwidth 0

set menu .menu.file
xmenu $menu
.menu add cascade -label "File" -underline 0 -menu $menu
$menu add command -label "New file" -underline 0 -command "file::NewFile"
$menu add command -label "Open..." -accelerator [gbn key-load] -underline 0 -command "file::Load file"
$menu add command -label "Save" -accelerator [gbn key-save] -underline 0 -command { file::Save $window($current_window,info) -force }
$menu add command -label "Save As..." -accelerator [gbn key-saveas ] -underline 4 -command { file::Save $window($current_window,info) }
$menu add command -label "Save All" -accelerator [gbn key-saveall ] -underline 4 -command { file::SaveAll }

$menu add cascade -label "Recent..." -menu $menu.rcent -underline 0
xmenu $menu.rcent 

$menu add command -label "Execute" -underline 0 -command "exec::execute"
$menu add separator
$menu add command -label "Close" -accelerator [gbn key-closefile] -underline 0 -command { file::Close $current_window }
$menu add command -label "Close All" -underline 0 -command { file::CloseAll }
$menu add command -label "Exit" -accelerator [gbn key-exit] -underline 1 -command file::eexit

set menu .menu.editmenu
xmenu $menu 
.menu add cascade -label "Edit" -underline 0 -menu $menu 
$menu add command -label "Undo" -accelerator [gbn key-undo ] -underline 0 -command "txt::undo .text"
$menu add command -label "Copy" -accelerator [gbn key-copy] -underline 0 -command "txt::copy .text"
$menu add command -label "Copy from..." -accelerator [gbn key-copyfromclip ] -underline 5 -command "txt::copyfrom_clip .text"
$menu add command -label "Cut" -accelerator [gbn key-delete ] -underline 1 -command "txt::cut .text"
$menu add command -label "CopyCut" -accelerator [gbn key-copycut ] -command "txt::copycut .text"
$menu add command -label "Paste" -accelerator [gbn key-paste ] -underline 0 -command "txt::paste .text"
$menu add command -label "Select all" -accelerator [gbn key-tagall ] -underline 0 -command "txt::tagall .text"
$menu add command -label "Evaluate" -accelerator [gbn key-texteval ] -underline 0 -command "txt::texteval .text"

$menu add separator
$menu add checkbutton -label "Wrap lines" -variable wraping -offvalue "none" -onvalue "word" -command { .text configure -wrap $wraping }   

set menu .menu.searchmenu
xmenu $menu 
.menu add cascade -label "Search" -underline 0 -menu $menu 
$menu add command -label "Find" -underline 0 -accelerator [gbn key-find ] -command "Find find"
$menu add command -label "Replace" -underline 0 -command "Find replace"
$menu add command -label "FindNext" -accelerator [gbn key-searchagain ] -underline 4 -command FindIt
$menu add command -label "Goto line" -underline 0 -accelerator [gbn key-goto ] -command Goto_line_ask

set menu .menu.windowmenu
xmenu $menu 
.menu add cascade -label "Windows" -underline 0 -menu $menu      

set menu .menu.helpmenu
xmenu $menu 
.menu add cascade -label "Help" -underline 0 -menu $menu 
$menu add command -label "Help" -command "win::activate 300"
$menu add separator
$menu add command -label "About" -hidemargin 1 -command { win::activate 300; pml::inserttext .text [pml::gettopic about] }

. configure -menu .menu
.menu configure -background $c(color-menubg) -foreground $c(color-menutxt) -activebackground $c(color-menuactive) -activeforeground $c(color-menuactivetext)


