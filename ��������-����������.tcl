# -*- coding: utf-8; -*- 
# по мотивам http://wiki.tcl.tk/560
# Здесь гвоздями прибито имя .ЭкраннаяКлавиатура, будьте осторожны!
namespace eval ::clcon {

#-keys range: list of (decimal or hex Unicodes of) characters to display. Consecutive sequences may be written as range, e.g. {0x21-0x7E} gives the printable lower ASCII chars.
#-keysperline n: number of keys per line, default: 16.
#-title string: If not "", text of a title label displayed above the keys. Default: "".
#-dir direction: if "r2l", moves cursor one to the left after each keypress. Useful for Arab/Hebrew. Default: l2r.
#-receiver widgetpath: Name of a text widget to receive the keystrokes at its insert cursor.

 variable МакетКлавиатуры { 1× | 2÷ | 
  | qQ | wW |
 }

 proc keyboard {w args} {
   variable МакетКлавиатуры
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
   foreach i ${МакетКлавиатуры} {
      if {$i == "|"} continue
#   foreach i [clist2list $opts(-keys)] 
      set c [string range $i 1 1]
      puts $c
      set cmd "$opts(-receiver) insert insert [list $c]"
      if {$opts(-dir)=="r2l"} {
         append cmd ";$opts(-receiver) mark set insert {insert - 1 chars}"
      } ;# crude approach to right-to-left (Arabic, Hebrew) 
      append cmd ";destroy .ЭкраннаяКлавиатура"
      button $w.k$i -text $c -command $cmd  -padx 5 -pady 0
      set Key [string cat "<Key-" [string range $i 0 0] ">"]
      ::clcon_key::b bind .ЭкраннаяКлавиатура $Key $cmd
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

 # Правьмя. Нужно передавать команду для вставки нажатой кнопки в приёмник. 
 # Например, с помощью этой клавиатуры можно вставить текст в историю консоли, что, очевидно, 
 # неверно. Также можно вставить букву в замороженный текст. 
 # А также нужно, чтобы клавиатура всплывала  не на соседнем мониторе, а вблизи курсора клавиатуры ПРАВЬМЯ
 proc Показать_экранную_клавиатуру {Приёмник} {
     set w .ЭкраннаяКлавиатура
     destroy $w 
     toplevel $w
     wm title $w "Экранная клавиатура для ${Приёмник}"
     bind $w <Escape> [list destroy $w]
     ::clcon_key::b bind $w <Control-Key-W> [list destroy $w]
     # экранная клавиатура где-то здесь: ×÷Ø№ₒ
     pack [::clcon::keyboard $w.kbd -title "Ввод значков для ${Приёмник}" -keys {0xD7 0xF7 0xD8 0x2116 0x2092} -receiver ${Приёмник}]
     # - {0x410-0x44f} - кириллица
     ::gui_util::FocusWindowByName $w
     grab $w
    } 
}
