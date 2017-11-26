## Закладки редактора

namespace eval ::ред-закладки {

  ## В будущем надо повесить крюк на закрытие файла, чтобы превращать
  ## тег в смещение. 

  ## Только имена закладок (назначаются пользователем)
  variable СписокИмёнЗакладок [list]
  ## Списки из имени буфера и имени тега закладки
  ## Имя тега закладки назначается средой
  variable СписокДанныхЗакладок [list]
  variable СчётчикЗакладок 0

  ## Стираем закладки от закрытых буферов
  proc СтеретьУстаревшиеЗакладки {} {
      # Ничего пока не делаем
  }
  
  proc ПоставитьЗакладкуЗдесь {} {
      variable СписокИмёнЗакладок 
      variable СписокДанныхЗакладок
      variable СчётчикЗакладок
      foreach {Статус ИмяЗакладки} [::LameAskForLispExpression {} "Введите имя закладки"] { break }
      if { ${Статус} eq "ok"} {
        УдалитьЗакладкуПоИмениЕслиОнаЕсть ${ИмяЗакладки}
        set ИмяТега "ТегЗакладки[incr СчётчикЗакладок]"
        set Би [::edt::cBi]
        set Текст [::edt::Bi2btext ${Би}]
        set СписокИмёнЗакладок [linsert ${СписокИмёнЗакладок} 0 ${ИмяЗакладки}]
        set СписокДанныхЗакладок [linsert ${СписокДанныхЗакладок} 0 [list ${Би} ${ИмяТега}]]
        ${Текст} tag add ${ИмяТега} [ ${Текст} index insert ] [ ${Текст} index {insert +2c} ]
        ${Текст} tag configure ${ИмяТега} -background Yellow
      }
  }

  proc УдалитьЗакладкуПоИмениЕслиОнаЕсть {ИмяЗакладки} {
      variable СписокИмёнЗакладок 
      variable СписокДанныхЗакладок 
      set index [lsearch -exact ${СписокИмёнЗакладок} ${ИмяЗакладки}]
      if { $index != -1 } {
          set СписокИмёнЗакладок [lreplace ${СписокИмёнЗакладок} $index $index]
          set СписокДанныхЗакладок [lreplace ${СписокДанныхЗакладок} $index $index]
      }
  }

  ## Показываем меню закладок и позволяем перейти к одной из них
  proc ВыбратьЗакладкуИПерейтиКНей {} {
    variable СписокИмёнЗакладок 
    variable СписокДанныхЗакладок 
    catch { destroy .СписокЗакладок }
    set w .СписокЗакладок
    set ww .СписокЗакладок.myList
    toplevel $w
    wm title $w "$w"
    listbox $ww
    set cmd {::ред-закладки::ПерейтиКЗакладке [.СписокЗакладок.myList get active] ; break}
    bind $ww <Return>  $cmd
    bind $ww <Double-1> $cmd
    bind $w <Escape> [list destroy $w ]
    bind $ww <Escape> "destroy $w; break "
    grid $ww -row 0 -column 0 -sticky news
    $ww insert 0 {*}${СписокИмёнЗакладок}
    raise $w
    focus $ww
    grab $w
  }
  
  ## Элементы закладки
  proc ПерейтиКЗакладке {ИмяЗакладки} {
    variable СписокИмёнЗакладок 
    variable СписокДанныхЗакладок 
    destroy .СписокЗакладок
    set index [lsearch -exact ${СписокИмёнЗакладок} ${ИмяЗакладки}]
    foreach {Б З} [lindex ${СписокДанныхЗакладок} $index] {break}
    [::edt::Bi2btext ${Б}] mark set insert ${З}.first
    [::edt::Bi2btext ${Б}] see insert
    ::edt::SwitchToBuffer ${Б}
  }
}

