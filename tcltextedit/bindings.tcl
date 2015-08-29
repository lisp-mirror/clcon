#?-Edit
#?
#? %2Abstract:%
#? Description for all text edit functions
#?
#? %fblue%Function             Name in rc file            Default key binding %fblack% 
#? Cut                    key-delete        Delete
#? Copy                   key-copy          Control-Insert 
#? Paste                  key-paste         Shift-Insert
#? Copy And Cut           key-copycut       Control-Delete
#? Delete line            key-delline       Control-Y
#? %lEvaluate%               key-texteval      Control-E
#? Delete to end of line  key-deltoeol      Control-T
#? Delete to end of word  key-deltoeow      Control-W
#? %LCopy_from...%           key-copyfromclip  Alt-Insert
#? Tag all                key-tagall        Alt-A
#? Next window            key-nextwin       Alt-N
#? Previous window        key-prevwin       Alt-P
#? Go to clipboard        key-clipboard     Alt-B
#? Exit editor            key-exit          Alt-X
#? Save                   key-save          F2
#? SaveAll                key-saveall       Control-F2
#? Load                   key-load          F3
#? Duplicate line         key-dupline       F4
#? Find                   key-find          F7
#? Find Again             key-searchagain   Alt-F7
#? %lSearchSelection%        key-searchsel     F9
#? Goto line              key-goto          F12
#? Save As                key-saveas        Alt-F2
#? Close                  key-closefile     Alt-F3
#? BackSpace              key-backspace     BackSpace
#? %LUndo%                   key-undo          Control-Z
#? 
#?
#? Related topics: %lRC%


#?-Copy_from...
#?
#? When used, a file request is presented, and the user is asked
#? to select a file.
#? When selected the file will be loaded and inserted into the
#? Clipboard
#?
#? Use Paste command to insert text into the current open file.
#?

#?-SearchSelection
#?
#? Mark the text you want to search for and 
#? press the key combination for SearchSelection
#?

#?-Evaluate
#?
#? Mark the area you want to evaluate and press the key combination for evaluate.
#? 
#? %1Example.% 
#? mark  1+2+3
#? Hit Evaluate key combination
#? and "1+2+3" is replaced with 6
#?

## Next needed for all menu shortcut
bind .text <Alt-KeyPress> win::updateln

bind .text <Escape> {
    win::updateln
    .menu.editmenu unpost
    .menu.searchmenu unpost
}

bind .text <KP_Enter> win::updateln

### force Text's bindings call before .text's bindings
### Needed for linenumber: first move the cursor (Text) and then updatln (.text)
### Don't seem to change something.
bindtags .text "Text .text . all"
####

bind Menu <Escape> {
    tkMenuEscape %W
    .menu unpost
    focus .text
}

proc xbind { what key to } {
    set l [split $key "|"]
    foreach n $l {
        bind $what <$n> $to
    }
}

#####################################################################################
bind .text <KeyPress>	{ txt::keypress  %A %K }

#Why this ???
# bind  Text <KeyPress>	{ tkTextInsert %W %A }	


bind .text <Button-3>	{.menu.editmenu post [winfo pointerx . ] [winfo pointery .]}
bind .text <Shift-Button-3>	{.menu.searchmenu post [winfo pointerx . ] [winfo pointery .]}
bind .text <Button>	{ win::updateln }
bind .text <Button-1>	{ .menu.editmenu unpost 
    .menu.searchmenu unpost 
}


#Remove some unwanted bindings
bind Text <<Cut>>   { }
bind Text <<Clear>> { }
bind Text <Insert>  { }
bind Text <<Copy>>  { }
bind Text <<Paste>> { }



#Shift bindings
xbind Text $c(key-paste)  	{ txt::paste %W }
xbind Text $c(key-copy)	        { txt::copy %W }
xbind Text $c(key-copycut) 	{ txt::copycut %W }
xbind Text $c(key-delline)       { txt::deleteline %W }
xbind Text $c(key-texteval)	{ txt::texteval %W }
xbind Text $c(key-deltoeol)	{ txt::deleol %W }
xbind Text $c(key-deleow)	{ txt::deleow %W }
xbind .text $c(key-undo)              {txt::undo %W }
xbind .text $c(key-copyfromclip) 	{ txt::copyfrom_clip %W }
xbind  Text $c(key-tagall)		{ txt::tagall %W }
xbind .text $c(key-nextwin)      	{ win::Next }
xbind .text $c(key-prevwin)      	{ win::Prev }
xbind .text $c(key-clipboard)      	{ "win::activate 100" }
xbind  Text $c(key-exit)		{ file::eexit }
xbind Text $c(key-delete)               { txt::cut %W }
xbind Text $c(key-backspace) 		{ txt::backspace %W }

#Fkey bindings
xbind .text $c(key-save)		{ file::Save $window($current_window,info) -force }
xbind  Text $c(key-load)  	        { file::Load file } 
xbind  Text $c(key-dupline)	        { txt::dupline %W }
xbind  Text $c(key-find)  		{ Find find }
xbind  Text $c(key-searchsel) 		{ txt::searchsel %W }
xbind .text $c(key-goto)		{ Goto_line_ask }


#Alt-F key bindings
xbind  Text $c(key-saveas)	{ file::Save $window($current_window,info) }
xbind  Text $c(key-searchagain) { global SearchString
    if {$SearchString==""} {Find find } else { FindIt } 
}
xbind Text $c(key-closefile)    { file::Close $current_window }
xbind Text $c(key-saveall)      { file::SaveAll }

# Bind to wheelmose
bind Text <Button-5> 		[list %W yview scroll 5 units]
bind Text <Button-4> 		[list %W yview scroll -5 units]
bind Text <Shift-Button-5> 	[list %W yview scroll 1 units]
bind Text <Shift-Button-4> 	[list %W yview scroll -1 units]
bind Text <Control-Button-5> 	[list %W yview scroll 1 pages]
bind Text <Control-Button-4> 	[list %W yview scroll -1 pages]


bind Text <F10> {puts stdout [win::names] }
#-------------------- Bindings for Entry -------------------

# Remove som unwanted bindings
bind Entry <<Copy>>  { }
bind Entry <<Paste>> { }

bind Entry <$c(key-copy)> {
    clipboard clear -displayof %W
    catch {
	clipboard append -displayof %W \
	    [string range [%W get] [%W index sel.first]\
		 [expr [%W index sel.last] - 1]]
    }
}


bind Entry <$c(key-paste)> {
    global tcl_platform
    catch {
	if {"$tcl_platform(platform)" != "unix"} {
	    catch {
		%W delete sel.first sel.last
	    }
	}
	%W insert insert [selection get -displayof %W -selection CLIPBOARD]
	tkEntrySeeInsert %W
    }
}


