# Сочетания с KP (кнопки на цифровой клавиатуре) непонятны линуксу. Пытаемся исправить. См. также SetKPBindingsForText

# Кусок из /home/den73/yar/tcl-8.6.4/lib/tk8.6/tk.tcl

if {[tk windowingsystem] eq "x11"} {

	event add <<NextChar>>		<KP_Right>
	event add <<SelectNextChar>>	<Shift-KP_Right>
	event add <<PrevChar>>		<KP_Left>
	event add <<SelectPrevChar>>	<Shift-KP_Left>
	event add <<NextWord>>		<Control-KP_Right>
	event add <<SelectNextWord>>	<Control-Shift-KP_Right>
	event add <<PrevWord>>		<Control-KP_Left>
	event add <<SelectPrevWord>>	<Control-Shift-KP_Left>
	event add <<LineStart>>		<KP_Home>
	event add <<SelectLineStart>>	<Shift-KP_Home>
	event add <<LineEnd>>		<KP_End>
	event add <<SelectLineEnd>>	<Shift-KP_End>
	event add <<PrevLine>>		<KP_Up>
	event add <<NextLine>>		<KP_Down>
	event add <<SelectPrevLine>>	<Shift-KP_Up>
	event add <<SelectNextLine>>	<Shift-KP_Down>
	event add <<PrevPara>>		<Control-KP_Up>
	event add <<NextPara>>		<Control-KP_Down>
	event add <<SelectPrevPara>>	<Control-Shift-KP_Up>
	event add <<SelectNextPara>>	<Control-Shift-KP_Down>

        if { [bind TablelistBody <Key-Prior>] eq {} } {
            ::tkcon::myerror "Нужно было загрузить Tablelist до fix_kp_in_linux.tcl"
        } 
        foreach pair {
                      {<Control-Key-KP_Next> <Control-Key-Next>}
                      {<Control-Key-KP_Prior> <Control-Key-Prior>}
                      {<Key-KP_Prior> <Key-Prior>} 
                      {<Key-KP_Next> <Key-Next>}
                      {<Control-Shift-Key-KP_End> <Control-Shift-Key-End>}
                      {<Control-Shift-Key-KP_Home> <Control-Shift-Key-Home>}
                      {<Control-Key-KP_End> <Control-Key-End>}
                      {<Control-Key-KP_Home> <Control-Key-Home>}
                      } {
             foreach {kp_key normal_key} $pair { 
                 bind TablelistBody $kp_key [bind TablelistBody $normal_key] }
            }
    }
