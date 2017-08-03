# -*- coding: utf-8; -*- 
# по мотивам http://wiki.tcl.tk/560
# Здесь гвоздями прибито имя .ЭкраннаяКлавиатура, будьте осторожны!
namespace eval ::clcon {

#-title string: If not "", text of a title label displayed above the keys. Default: "".
#-receiver widgetpath: Name of a text widget to receive the keystrokes at its insert cursor.

 variable МакетКлавиатуры {
|`~|1☼|2@|3#|4$|5°|6^|7&|8₽|9(|0)|-N|=+|
| |qQ|wW|e×|r†|tt|yy|uu|ii|oo|pp|[откр-фигурная-скобка|]закр-фигурная-скобка|  |
|  |aa|ss|dₒ|ff|gg|h÷|jj|kλ|ll|::|;;|""|\вертикальная-черта|
|   |zz|xx|c♥|vₓ|b•|nØ|mm|,,|..|//|
}


 proc keyboard {w args} {
   variable МакетКлавиатуры
   frame $w
   array set opts { -title "" -receiver "" }
   array set opts $args ;# no errors checked 
   set klist {}; set n 0
   if {$opts(-title)!=""} {
      grid [label $w.title -text $opts(-title) ] 
      }
   set j 0
   array set seen {}
   set r 1
   frame $w.row$r
   grid $w.row$r -sticky w
   foreach i [split ${МакетКлавиатуры} "|"] {
      set i [string map {вертикальная-черта "|"} $i]
      set i [string map {откр-фигурная-скобка "\{"} $i]
      set i [string map {закр-фигурная-скобка "\}"} $i]
      set c [string index [string trim $i] 1]      
      set key [string index [string trim $i] 0]
      if {$key == "`"} {set key quoteleft}
      if {$key == "-"} {set key minus}
      if {$key == "\""} {set key quoteright}
      if {$key == ","} {set key comma}
      if {$key == "."} {set key period}
      if {$key == "/"} {set key slash}
      set empty [expr {$key == ""}]
      set newline [expr {"\n" eq [string index $i [expr [string length $i]-1]]}]
      if {$newline} {
        set i [string range $i 0 [expr [string length $i]-2]]
      }
      if {!$empty && [info exists seen($key)]} {
        error "Кнопка $key уже есть"
      }
      set seen($key) 1
      set cmd "$opts(-receiver) insert insert [list $c]"
      append cmd ";destroy .ЭкраннаяКлавиатура"
      if {$newline} {
        if {!($klist == "")} {
          eval grid $klist -sticky news
          set n 0; set klist {}
          set r [expr $r + 1]
          frame $w.row$r
          grid $w.row$r -sticky w
        }
      } else {
        if {[string trim $i] == ""} {
           button $w.row$r.k$j -text $i -padx 0 -pady 0 -font $::tkcon::OPT(font)
        } else {
           button $w.row$r.k$j -text $i -command $cmd  -padx 0 -pady 0 -font $::tkcon::OPT(font)
           set Key [string cat "<Key-" $key ">"]
           if {!$empty} {
             ::clcon_key::b bind .ЭкраннаяКлавиатура $Key $cmd
           }
        }
        lappend klist $w.row$r.k$j
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
     pack [::clcon::keyboard $w.kbd -title "Ввод значков для ${Приёмник}" -receiver ${Приёмник}]
     ::gui_util::FocusWindowByName $w
     grab $w
    } 
}
