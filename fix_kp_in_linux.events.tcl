# Сочетания с KP (кнопки на цифровой клавиатуре) непонятны линуксу. Пытаемся исправить. См. также SetKPBindingsForText
# См. также fix_kp_in_linux.tablelist.tcl
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
	event add <<SelectLineStart>>	<Shift-KP_7>
	event add <<LineEnd>>		<KP_End>
	event add <<SelectLineEnd>>	<Shift-KP_End>
	event add <<SelectLineEnd>>	<Shift-KP_1>
	event add <<PrevLine>>		<KP_Up>
	event add <<NextLine>>		<KP_Down>
	event add <<SelectPrevLine>>	<Shift-KP_Up>
	event add <<SelectPrevLine>>	<Shift-KP_9>
	event add <<SelectNextLine>>	<Shift-KP_Down>
	event add <<SelectNextLine>>	<Shift-KP_3>
	event add <<PrevPara>>		<Control-KP_Up>
	event add <<PrevPara>>		<Control-KP_9>
	event add <<NextPara>>		<Control-KP_Down>
	event add <<NextPara>>		<Control-KP_3>
	event add <<SelectPrevPara>>	<Control-Shift-KP_Up>
	event add <<SelectPrevPara>>	<Control-Shift-KP_9>
	event add <<SelectNextPara>>	<Control-Shift-KP_Down>
	event add <<SelectNextPara>>	<Control-Shift-KP_3>
}
