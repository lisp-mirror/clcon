# -*- coding: utf-8; -*- 
# по мотивам http://wiki.tcl.tk/560
# Здесь гвоздями прибито имя .ЭкраннаяКлавиатура, будьте осторожны!
namespace eval ::clcon {

#-keys range: list of (decimal or hex Unicodes of) characters to display. Consecutive sequences may be written as range, e.g. {0x21-0x7E} gives the printable lower ASCII chars.
#-keysperline n: number of keys per line, default: 16.
#-title string: If not "", text of a title label displayed above the keys. Default: "".
#-dir direction: if "r2l", moves cursor one to the left after each keypress. Useful for Arab/Hebrew. Default: l2r.
#-receiver widgetpath: Name of a text widget to receive the keystrokes at its insert cursor.

 variable УскорителиДляЭкраннойКлавиатуры {1 2 3 4 5 6 7 8 9 0}

 proc keyboard {w args} {
   variable УскорителиДляЭкраннойКлавиатуры
   frame $w
   array set opts {
      -keys {0x21-0x7E} -title "" -keysperline 16 -dir l2r -receiver ""
   }
   array set opts $args ;# no errors checked 
   set klist {}; set n 0
   if {$opts(-title)!=""} {
      grid [label $w.title -text $opts(-title) ] \
               -sticky news -columnspan $opts(-keysperline)
      }
   set j 0
   foreach i [clist2list $opts(-keys)] {
      set c [format %c $i]
      set cmd "$opts(-receiver) insert insert [list $c]"
      if {$opts(-dir)=="r2l"} {
         append cmd ";$opts(-receiver) mark set insert {insert - 1 chars}"
      } ;# crude approach to right-to-left (Arabic, Hebrew) 
      append cmd ";destroy .ЭкраннаяКлавиатура"
      button $w.k$i -text $c -command $cmd  -padx 5 -pady 0
      set Key [string cat "<Key-" [lindex ${УскорителиДляЭкраннойКлавиатуры} $j] ">"]
      bind .ЭкраннаяКлавиатура $Key $cmd
      lappend klist $w.k$i
      if {[incr n]==$opts(-keysperline)} {
        eval grid $klist -sticky news
        set n 0; set klist {}
      }
      set j [expr $j + 1]
    }
    if [llength $klist] {eval grid $klist -sticky news}
    set w ;# return widget pathname, as the others do
 }
 proc clist2list {clist} {
    #-- clist: compact integer list w.ranges, e.g. {1-5 7 9-11}
    set res {} 
    foreach i $clist {
        if [regexp {([^-]+)-([^-]+)} $i -> from to] {
            for {set j [expr $from]} {$j<=[expr $to]} {incr j} {
                lappend res $j
            }
        } else {lappend res [expr $i]}
    }
    set res
 }

 proc ПоказатьКлавиатуру {Приёмник} { 
     destroy .ЭкраннаяКлавиатура 
     toplevel .ЭкраннаяКлавиатура
     wm title .ЭкраннаяКлавиатура "Экранная клавиатура для ${Приёмник}"
     pack [keyboard .ЭкраннаяКлавиатура.kbd -title {Ввод значков} -keys {0xD7 0xF7 0xD8 0x2116} -receiver ${Приёмник}]
     ::gui_util::FocusWindowByName .ЭкраннаяКлавиатура
     # - {0x410-0x44f} - кириллица
 }
 

}