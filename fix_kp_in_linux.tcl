# Сочетания с KP (кнопки на цифровой клавиатуре) непонятны линуксу. Пытаемся исправить. См. также SetKPBindingsForText

# Кусок из /home/den73/yar/tcl-8.6.4/lib/tk8.6/tk.tcl

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
