#?-RC
#? 
#? %2Abstract:%
#? The configuration file is called rc, and it is
#? located in $HOME/.tcltextedit/
#? The rc file contains these items:
#? 
#? Color settings,
#? Font settings,
#? Key definitions,
#? File types.
#? And some parameters that the editor uses to keep track of different parameters.

namespace eval  cfg {

namespace export cset load save file del checkcfg

set cfglist "
{aa}                      {Configuration file for $prgname}
{color-background}		{Gray76}
{color-editbg}	          {White}
{color-edittxt}		  {Black}
{color-filelist}	  {Blue}
{color-menuactivetext}	  {White}
{color-menubg}	 	  {Gray76}
{color-menutxt}		  {Black}
{color-menuactive}		  {#008080}
{color-statustext}	  {Blue}
{color-statustextchanged} {Red}
{color-cursor}            {Black}
{filetypes}		  { {{All Files} {*} } {{Text Files} {.txt}} {{TCL Scripts} {.tcl}} {{HTML Scripts} {.html}} }
{geometry}		  {=76x26}
{numrecent}		  {6}
{recent1}		  {}
{recent2}		  {}
{recent3}		  {}
{recent4}		  {}
{recent5}		  {}
{recent6}		  {}
{speed}                   {HelloWorld TCL-Exec}
{tearoff}		  {0}
{key-paste}		{Shift-Insert}
{key-copy}		{Control-Insert}
{key-copycut}		{Control-Delete}
{key-delline}		{Control-y|Control-Y}
{key-texteval}		{Control-e|Control-E}
{key-deltoeol}		{Control-t|Control-T}
{key-deleow}		{Control-w|Control-W}
{key-copyfromclip}	{Alt-Insert}
{key-tagall}		{Alt-a|Alt-A}
{key-nextwin}		{Alt-n|Alt-N}
{key-prevwin}		{Alt-p|Alt-P}
{key-clipboard}		{Alt-b|Alt-B}
{key-exit}		{Alt-x|Alt-X}
{key-save}              {F2}
{key-saveall}           {Control-F2}
{key-load}              {F3}
{key-dupline}           {F4}
{key-find}              {F7}
{key-searchsel}         {F9}
{key-goto}		{F12}
{key-saveas}            {Alt-F2}
{key-searchagain}       {Alt-F7}
{key-closefile}         {Alt-F3}
{key-delete}            {Delete}
{key-backspace}         {BackSpace}
{key-undo}              {Control-z|Control-Z}
{font-editor}           {system}
"

proc checkcfg {} {
global cfg::cfglist c debug_messages
set num [expr [llength $cfg::cfglist] / 2]
for {set i 0} {$i < $num } {incr i } {
 set a "[lindex $cfg::cfglist [expr $i*2] ]"
 set b "[lindex $cfg::cfglist [expr $i*2+1] ]"
 if {![info exist c($a)]} { 
 if {$debug_messages=="1"} { puts stdout "Adding $a with value $b to Config file" }
 set c($a) $b
 }
}
}


proc del {n} {
global c
set c($n) "*del*"
cfg::save
}

proc cset {n v} {
global c 
c "$n $v"
set c($n) $v
cfg::save
}

proc load {} {
global c cfg::file 
if ![file exists $cfg::file] {
                            bgerror "The configuration file $cfg::file \nis missing creating a new"
                            cfg::checkcfg
                            cfg::save
			    return 0
                           }


set f [open $cfg::file "RDONLY" ]
while {![eof $f]} {
set a [gets $f]
set c([lindex $a 0]) [lindex $a 2]
}
close $f         
cfg::checkcfg
}

proc save {} {
global c cfg::file
set f [open $cfg::file w]
set k [lsort [array names  c ]]
for {set i 0} {$i <[llength $k] } {incr i } {
if {$c([lindex $k $i])!="*del*"} {
 puts $f "{[lindex $k $i]} = {$c([lindex $k $i])}"
 }
}
close $f
}

}
