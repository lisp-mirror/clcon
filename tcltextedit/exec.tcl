#?-Execute
#?  
#? Executes the command entered in the input field
#?
#? %2Formating:%
#? %%f  -  Current open file
#? %%p  -  Path to current open file
#?
#? %2Example:%
#? "xterm -e joe %%f &" 
#? Starts an xterm with the editor joe and loads the current open file.
#?
#? 

namespace eval exec {

namespace export execute RunProgram

proc RunProgram { cmd } {
global window current_window
set result ""
set i 0

macro::rec exec::RunProgram "$cmd"

c $cmd

while {$i < [string length $cmd]} {
set cc [string index $cmd $i]

if {$cc=="%"} { 
incr i
set cc "$cc[string index $cmd $i]"

switch "$cc" {
 "%f" {set cc $window($current_window,name)}
 "%F" {set cc $window($current_window,name)}
 "%p" {set cc [file dirname $window($current_window,name)] }
 "%P" {set cc [file dirname $window($current_window,name)] }
 }
set result "$result$cc"

} else {
set result "$result$cc"
}

incr i
}

c $result
update
set a [lindex $result 0]
set b [lrange $result 1 end]
if {$b==""} { eval "exec {$a}" } else { eval "exec {$a} {$b}" }
}


proc execute {} {
global c
variable exec

c 

set ou .ou
catch {destroy $ou}
toplevel $ou
wm title $ou "Execute"

frame $ou.f
frame $ou.f.fr1
frame $ou.fr2
pack $ou.f.fr1  -side top
pack $ou.f $ou.fr2 -side left

label $ou.f.fr1.lab -text "Command:"
xentry $ou.f.fr1.ent -textvariable cmd -width 30
pack $ou.f.fr1.lab $ou.f.fr1.ent -side left -pady 10 -padx 10
$ou.f.fr1.ent selection range 0 end
xbutton $ou.fr2.ok -text Execute -width 10 -command { 
destroy .ou
exec::RunProgram $cmd
}

xbutton $ou.fr2.can -text Cancel -width 10 -command "destroy $ou"
pack $ou.fr2.ok $ou.fr2.can -side top -padx 10 -pady 5

focus $ou.f.fr1.ent

bindtags $ou.f.fr1.ent "Entry $ou.f.fr1.ent $ou . all" 

bind $ou.f.fr1.ent <Return> { 
destroy .ou
exec::RunProgram $cmd
}
bind $ou.f.fr1.ent <KP_Enter> {
exec::RunProgram $cmd 
}
bind $ou.f.fr1.ent <Escape> "destroy $ou"
grab $ou
powin $ou
}                                          
}
