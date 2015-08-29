#?-Macros
#?
#? Tcltextedit has the ability to record and play macros.
#? Macros are by default stored in $HOME/.tcltextedit/???.mac
#? where ??? stands for the name of the macro.
#? The macro files are plain tcl scripts, that makes it possible
#? for any person with knowledge of tcl to customize your own macros.
#?
#? %2Recording:%
#? You record macros by selecting the menu: Macro->Record Macro.
#? When recording is activatef text appear in the status line telling
#? you that the recording mode is active.
#? 
#? The just use the editor as usual and all your actions will be recorded.
#? 
#? To stop recording shoose: Macro->Stop recording, and a prompt will
#? appear asking you to shoose a name for your macro.
#? When you hit the Ok button your macro will be stored.
#?
#? Related topics: %lSPEEDBUTTONS%
#? 

namespace eval macro {

namespace export rec play new buttonconf update names files


proc names {} {
global home
set k [glob -nocomplain "$home/*.mac"]
set n ""
for {set i 0} {$i <[llength $k] } {incr i } { 
set n "$n {[file rootname [file tail [lindex $k $i]]]}" 
}
return $n
}

proc update {} {
global c home

.menu.macromenu delete 0 end
.menu.macromenu add command -label "Record macro" -underline 0 -accelerator "($c(key-newmacro))" -command macro::new
.menu.macromenu add separator

set k [macro::names]

foreach n $k {
.menu.macromenu add command -label $n -command "macro::play {$n}"
}

}

proc stop {} {
global macro_rec
set macro_rec 0
 .menu.macromenu entryconfigure [.menu.macromenu index "Stop recording"] -label "Record macro"
 .status.filler config -text ""
}

proc new {} {
global macro_data macro_rec cu_macro

if {!$macro_rec} {
 .menu.macromenu entryconfigure [.menu.macromenu index "Record macro"] -label "Stop recording"
 set macro_data ""
 set macro_rec 1
 .status.filler config -text "Recording.. (press F5 to stop)"
} else {
 

   switch [ macro::store] {
   ok       {macro::stop
             macro::msave
            }
   abort    {macro::stop}
   continue {}
   }
 
}
}


proc key {data} {
global current_window window

 set window($current_window,echange) 1
 set window($current_window,change) 1
 win::update

 switch $data  {
 Return	{.text insert insert "\n"}
 Alt_L 	{}
 Alt_R 	{}
 Shift_L    {}
 Shift_R   {}
 Alt_L       {}
 Control_L {}
 Control_R {}
 Caps_Lock {}
 Home      {tkTextSetCursor .text {insert linestart} }
 End       {tkTextSetCursor .text {insert lineend} }
 Mode_switch {}
 Delete    { txt::cut .text }
 BackSpace { txt::backspace .text }
 space     {.text insert insert " "}
 Right     { tkTextSetCursor .text  "insert + 1 chars"}
 Left      { tkTextSetCursor .text  "insert - 1 chars"}
 Up        { tkTextSetCursor .text  "insert - 1 lines"}
 Down      { tkTextSetCursor .text  "insert + 1 lines"}

 default   {   .text insert insert $data }
  }

.text see insert   

}

proc rec { cmd args } {
global macro_data macro_rec

if {$macro_rec} {

if {$args=="\r"} {set args "Return"}
if {$args=="\n"} {set args "Return"}
if {$args=="\b"} {set args "BackSpace"}
if {$args=="\x7f"} {set args "Delete"}

set macro_data "$macro_data$cmd $args\n"
c $cmd $args
}

}


proc msave {} {
global env macro_data home current_macro prgname
set n $current_macro
file delete -force "$home/$n.mac"
set f [open "$home/$n.mac" "CREAT WRONLY" ]

puts $f "###################################################"
puts $f "# Macro recorded by $prgname"
puts $f "###################################################\n"

puts $f $macro_data nonewline
close $f
macro::update
speed::Update
}


proc play {n} {
global macro_data cu_macro current_window installdir home
macro::rec macro::play $n
source "$home/$n.mac"
}

proc store {} {
global url current_window window c current_macro r c
set current_macro ""
set ou .ou
catch {destroy $ou}
toplevel $ou
wm title $ou "Save macro as:"
frame $ou.fr1
frame $ou.fr2
pack $ou.fr1 $ou.fr2 -side left
label $ou.fr1.lab -text "Name:"
xentry $ou.fr1.ent -textvariable current_macro
pack $ou.fr1.lab $ou.fr1.ent -side left -pady 10 -padx 10
$ou.fr1.ent selection range 0 end
xbutton $ou.fr2.ok -text "Ok" -width 10 -command "set r ok"
xbutton $ou.fr2.can -text "Abort" -width 10 -command "set r abort"
xbutton $ou.fr2.con -text "Continue.." -width 10 -command "set r continue"

pack $ou.fr2.ok $ou.fr2.con $ou.fr2.can -side top -padx 10 -pady 5
focus $ou.fr1.ent
bind $ou.fr1.ent <Return> "set r ok"
bind $ou.fr1.ent <KP_Enter> "set r ok"
bind $ou.fr1.ent <Escape> "set r continue"
grab $ou
powin $ou

vwait r
destroy $ou
return $r
}                                          


}