#?-Help
#? 
#? You are looking at it.
#?
#? %2Navigation%
#? 
#? Press INDEX to get to the index page 
#? Press BACK to go to the recently accessed page
#? Press FORWARD to go to the page you were on when you pressed the BACK button
#? To exit help choose anoter window in the Windows menu
#?
#? If you want to send me a message here is my mail address %lAbout%
#?

namespace eval pml {

namespace export  init gettopic hp history filen inserttext

proc init {} {
global hp toprow history installdir filen
set toprow "%bwhite%%ptop1.gif:index%%ptop2.gif:back%%ptop3.gif:forward%%ptop4.gif%\n"
for {set i 0} {$i <100 } {incr i } {
set history($i) ""
}
set hp 0
set filen "$installdir/helpfile"
}

proc getindex {} {
global toprow filen 
set result ""
set f [open $filen "r"]
while {![eof $f]} {
set s [gets $f]
if { [string first "#?-" $s ] == 0 } {  set result "$result {%i[string range $s 3 end ]%}"  }
}
close $f
set r ""
foreach n [lsort $result] {
set r "$r $n\n"
}
return "$toprow$r"
}



proc gettopic {args} {
global history hp

set topic [string toupper $args]

if {$topic==""} {

if {$hp>1} {
 set topic $history([expr $hp - 1])
 } else {
	set topic "INDEX"
	}
}

set result ""
c $topic
case $topic { 
 BACK   {  
	if { $hp > 1} {
	set hp [expr $hp -2 ]
        set result [ pml::gettext $history($hp) ]
	incr hp
   	 }
              }
 FORWARD {
	if {$history($hp)!=""} {
	set result [ pml::gettext $history($hp) ]
	incr hp
	}
	}
 default {
	set history($hp) $topic
	incr hp
	set result [pml::gettext $topic]
	}
 }
return $result
}


proc gettext { topic } {
global toprow filen
if {$topic=="INDEX"} { 
return [getindex]
}  
set result ""
set f [open $filen "r"]
while {![eof $f] && $result==""} {
set s [gets $f]
if { [string first "#?-" $s ] == 0 } { 
 if {$topic == [string toupper [string range $s 3 end ] ] } {
  set result $toprow
  set s [gets $f]
  while {![eof $f] && [string first "#?-" $s ] != 0} {
   set result "$result[string range $s 2 end ]\n"
   set s [gets $f]
   }
  }
 }
}
close $f
return $result
}

proc topics {} {
global filen
set result ""
set f [open $filen "r"]
while {![eof $f]} {
set s [gets $f]
if { [string first "#?-" $s ] == 0 } { 
 set result "$result {[string range $s 3 end ]}"
 }
}
close $f
return $result
}


proc press {w n} {
set s [pml::gettopic $n]
if {$s!=""} { pml::inserttext $w $s }
}

proc AddText {w s param fg} {
$w insert end $s
set  start "end - [string length $s] char"
$w tag add $s "$start-1 char" "end-1 char"
$w tag configure $s -font $param -foreground $fg
}

proc AddTextBind {w str param fg} {
set s [lindex [split $str ":" ] 0]
set n [lindex [split $str ":" ] 1]
if {$n==""} { set n $s }

$w insert end $s
set start "end - [string length $s] char"
$w tag add $s "$start-1 char" "end-1 char"
$w tag configure $s -font $param -foreground $fg
$w tag bind $s <Button-1> "pml::press $w $n"
}

proc AddTextButton {w n} {
$w insert end $n
set start "end - [string length $n] char"
$w tag add $n "$start-1 char" "end-1 char"
$w tag configure $n -background gray -foreground black -relief raised -borderwidth 1
$w tag bind $n <Button-1> "pml::press $w $n"
}

proc AddPicture {w str} {
global installdir
set n [lindex [split $str ":" ] 0]
set s [lindex [split $str ":" ] 1]
if {$s==""} { set s $n }

image create photo "$n" -file [file join "$installdir/$n"]
$w image create end -image "$n"
$w tag add $s "end-2 char" "end-1 char"
$w tag configure $s -font "Times 16" -background gray -foreground black -relief raised -borderwidth 1
$w tag bind $s <Button-1> "pml::press $w $s"
}

proc inserttext { w s } {
c ""

$w configure -state normal
set ctag "0"
set ttag ""

foreach n [$w tag names] {
$w tag delete $n
}

$w delete 1.0 end
$w config -cursor arrow

set i 0
while {$i<[string length $s]} {
 set c [string index $s $i]
 if {$c=="%"} {
  incr i
  set m ""
  set c "$c[string index $s $i]"
  if {$c!="%%"} {
  incr i
  while {[string index $s $i]!="\n" && $i<[string length $s] && [string index $s $i]!="%"} { 
   set m "$m[string index $s $i]"; 
   incr i 
   }
 case $c {
   %b { $w config -background $m }
   %B { $w config -background $m }
   %f { $w config -foreground $m }
   %F { $w config -foreground $m }
   %l { AddTextButton $w $m  }
   %L { AddTextButton $w $m  }
   %0 { AddText $w $m "Times 20 bold" blue  }
   %1 { AddText $w $m "Times 20 bold" red  }
   %2 { AddText $w $m "Times 12 bold" blue  }
   %i { AddTextBind $w $m "Times 14 bold" red }
   %I { AddTextBind $w $m "Times 14 bold" red }
   %p { AddPicture $w $m}
   %P { AddPicture $w $m}
   }
   } else { $w insert end "%" }
  } else { $w insert end $c }
 incr i
 }
$w configure -state disabled
}

}




#text .t -wrap none
#pack .t -fill both -expand yes
#set installdir "/usr/local/tcltextedit/help/"
#set installdir "C:/windows/Skrivbord/Help/"
pml::init
#pml::inserttext .t [pml::gettopic index]
