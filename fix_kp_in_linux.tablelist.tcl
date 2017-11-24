# Сочетания с KP (кнопки на цифровой клавиатуре) непонятны линуксу. Пытаемся исправить. См. также SetKPBindingsForText
# См. также fix_kp_in_linux.events.tcl

if {[tk windowingsystem] eq "x11"} {

    if { [bind TablelistBody <Key-Prior>] eq {} } {
        ::tkcon::myerror "Нужно было загрузить Tablelist до fix_kp_in_linux.tablelist.tcl"
    } 
    foreach pair {
                  {<Control-Key-KP_Next> <Control-Key-Next>}
                  {<Control-Key-KP_Prior> <Control-Key-Prior>}
                  {<Key-KP_Prior> <Key-Prior>} 
                  {<Key-KP_Next> <Key-Next>}
                  {<Control-Shift-Key-KP_End> <Control-Shift-Key-End>}
                  {<Control-Shift-Key-KP_Home> <Control-Shift-Key-Home>}
                  {<Control-Key-KP_End> <Control-Key-End>}
                  {<Control-Key-KP_Home> <Control-Key-Home>}} {
             foreach {kp_key normal_key} $pair { 
                 bind TablelistBody $kp_key [bind TablelistBody $normal_key] 
             }
        }
    }
