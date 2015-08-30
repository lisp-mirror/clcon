set debug_messages 0
set tcl_traceExec 0

set prgname "Tcl TextEdit 0.9.7"

#?-About
#?%2TCL TextEdit v0.9.x%
#?
#? %ptte.gif%
#? 
#? Created by Dennis Ehlin
#? mail: hkc141i@tninet.se
#? http://www.user.tninet.se/~hkc141i
#?
#? Related topics: %lDisclaimer% %lPromotion% 
#?
#?-Promotion
#?
#? Please feel free to use this logo at your homepage
#?
#? %pttenow.gif%
#?
#? Just copy the image "ttenow.gif " and insert the html code below where you want it
#? The image is included in the distribution. 
#?
#? <A HREF="http://user.tninet.se/~hkc141i/edit.html">
#? <IMG SRC="ttenow.gif" BORDER="0"
#? ALT="Get TCL TextEdit now!!"></A>
#?
#?-Disclaimer
#?
#?%2TCL TextEdit v0.9.x%
#?
#?Copyright (c) 1997-1999 Dennis Ehlin
#?
#?Permission is hereby granted, free of charge, to any person obtaining a copy
#?of this software and associated documentation files, to deal
#?in the Software without restriction, including without limitation the rights
#?to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#?copies of the Software, and to permit persons to whom the Software is
#?furnished to do so, subject to the following conditions:
#?
#?The above copyright notice and this permission notice shall be included in
#?all copies or substantial portions of the Software.
#?
#?THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#?IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#?FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
#?AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#?LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#?OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
#?THE SOFTWARE
#?
#?-Problems?
#?
#? If you have any problems or if you have any ideas, dont hesitate to
#? Send me a mail message.
#? 
#? You can find my mail address here %lAbout%
#?

# buddens'
proc tkTextSetCursor {text index} {
    $text mark set insert $index
}

proc bgerror {s} {
global rr debug_messages errorInfo errorCode installdir
set info $errorInfo

set ou .ou
catch {destroy $ou}
toplevel $ou
wm title $ou "Message"
wm protocol   $ou WM_DELETE_WINDOW "set rr 1"
set rr 0

if {[info exist installdir]} {
 if {[file exist [file join $installdir/stop.gif] ]} {
  image create photo warn -file [file join $installdir/stop.gif]
  label $ou.img -image warn -borderwidth 2 -relief raised
  pack $ou.img -side left
 } 
}

frame $ou.f 

label $ou.f.label -text "$s !"
button $ou.f.ok -text "Ok"       -command "set rr 1" -width 5 -borderwidth 1
pack $ou.f.label
pack $ou.f.ok  

if {$debug_messages>0} {
label $ou.f.d1 -text "Info: $info" -foreground red
label $ou.f.d2 -text "Code: $errorCode" -foreground blue
pack  $ou.f.d2 $ou.f.d1
}

pack $ou.f -side left
bind $ou <Escape> "set rr 1"
bind $ou <Return> "set rr 1"
grab $ou
focus $ou.f.ok
if {[info exist powin]} { powin $ou }
vwait rr
destroy $ou
}


#------------------------------------------------------------------------------+

proc setup {} {
global env installdir tk_version tcl_version c home numw tcl_platform

if {$tcl_platform(platform)!="windows"} {

if {![info exist env(DISPLAY)]} {
puts stdout "Are you sure yo're running X ?"
exit
}
}

#Check tk/tcl versions
if {$tk_version<8} { 
puts stdout "You need to upgrade to tk/tcl 8.0 or higer!"
exit
}
if {$tcl_version<8} { 
puts stdout "You need to upgrade to tk/tcl 8.0 or higer!"
exit
}

set cfg::file "$home/rc"
cfg::load

# Load images
image create photo warn -file [file join $installdir/stop.gif]

tk_setPalette background $c(color-background)
}


#------------------------------------------------------------------------------+


if {$tcl_platform(platform)!="windows"} {
#Unix
set installdir [file dirname [info script]]
set home "$env(HOME)/.tcltextedit"

if {![file exists $home]} {
file mkdir $home
}

} else {
#Windows
set installdir [file dirname [info script]]
set home $installdir
}

set numw 99
source $installdir/cfg.tcl
setup

for {set i 1} {$i <100 } {incr i } {
set window($i,name) "Untitled #$i"
set window($i,info) ""
set window($i,change) "0"
set window($i,pos) "1.0"
set window($i,echange) "0"
set window($i,extra) ""
set window($i,temp) ""
}
set i 100
set window($i,name) "ClipBoard"
set window($i,info) ""
set window($i,change) "0"
set window($i,pos) "1.0"
set window($i,echange) "0"
set window($i,extra) ""
set window($i,temp) "NoFile"


set i 200
set window($i,name) "FileList"
set window($i,info) ""
set window($i,change) "0"
set window($i,pos) "1.0"
set window($i,echange) "0"
set window($i,extra) ""
set window($i,temp) "NoFile"

set i 300
set window($i,name) "Help"
set window($i,info) ""
set window($i,change) "0"
set window($i,pos) "1.0"
set window($i,echange) "0"
set window($i,extra) ""
set window($i,temp) "NoFile"



set ftypes {
{{All Files}       {*}         }
{{Text Files}      {.txt}      } 
{{TCL Script}      {.tcl}      }
{{C Source}        {.c}        }
{{Pascal Source}   {.pas}      }
{{HTML Files}      {.htm .html} }
}


set current_window 1
set lw 1
set SearchPos "0.0"
set SearchString ""
set ReplaceString ""
set SearchLen "0"
set SearchString ""
set hold 0
set findcase 0
set SearchDir "forwards"
set Text_Insert 1
set marknums 0
set markon   0
set undo_active 1


wm iconbitmap . @$installdir/mini-textedit.xbm
wm iconname   . $prgname
wm title      . $prgname
wm minsize    .  56 1
wm protocol   . WM_DELETE_WINDOW { file::eexit }
wm geometry   . $c(geometry)


#source $installdir/supertext.tcl
source $installdir/mclistbox.tcl
source $installdir/window.tcl
source $installdir/help.tcl
source $installdir/findreplace.tcl
source $installdir/file.tcl
source $installdir/cmds.tcl
source $installdir/exec.tcl
source $installdir/gui.tcl
source $installdir/txt.tcl
source $installdir/bindings.tcl
#source $installdir/speed.tcl

#------------------------------------------------------------------------------+

#Check if user supplied anything at the command line

set i 1
set fil [lindex $argv 0]

while {$fil!=""} {

#NewFile

set tmp [string toupper $fil]
if {[string first "HTTP://" $tmp]=="0"} {
file::Load "http $fil" -force
} else {
	#textLoadFile .text $fil
        file::Load "file $fil" -force
	}

set fil [lindex $argv $i]
incr i
if {$i>$numw} {set fil ""}
}

if {$i==1} { file::NewFile }

win::activate 1
win::update
win::status_change

#Set up update procedure for status line
trace variable window w win::status_change

#Focus on text

updaterecent

grab .

focus .text


