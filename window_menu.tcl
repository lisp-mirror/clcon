## window menu, history menu и другие меню, применяемые более чем в одном месте

namespace eval ::window_menu {

    # From can be toplevel or non-toplevel window
    proc CanSwitchToToplevel {from to CanCreateThatWindow} {
        set top_from [winfo toplevel $from]
        if {$top_from eq $to} {
            return 0
        } elseif {$CanCreateThatWindow} {
            return 1
        } elseif {![winfo exists $to]} {
            return 0
        } elseif {[lsearch -exact {normal iconic zoomed} [wm state $to]] >= 0} {
            return 1
        } else {
            return 0
        }
    }
    
    # flagPtr is used to check that we have found disabled item while
    # filling the menu
    proc CalcEnabledForOneItem {windowToBeEnabled CanCreateThatWindow} {
        upvar SomeMenuDisabled SomeMenuDisabled
        upvar w_top w_top
        upvar state state
        if {[CanSwitchToToplevel $w_top \
               $windowToBeEnabled $CanCreateThatWindow]} {
            set state "normal"
       } else {
            set SomeMenuDisabled 1
            set state "disabled"
        }
    }

    proc WrapCmdForKeyboard {w windowToBeEnabled CanCreateThatWindow SwitchCmd} {
        return "if {\[[list ::window_menu::CanSwitchToToplevel $w $windowToBeEnabled $CanCreateThatWindow]\]} { $SwitchCmd }; break"
    }

    # We must create menu keybindings as we're preparing window
    # In contrast, menu itself is created dynamically when use activates menu bar
    # So we may need keybindings before we have menu created.
    # See also ::window_menu::DynamicWindowMenu
    proc WindowMenuKeyBindings {w SingleModBindtag DoubleModBindtag} {
        ## Window Menu
        ## w is a widget to accept keyboard bindings, 

        variable ::tkcon::PRIV

        ## many vars are upvar'ed in the CalcEnabledForOneItem
        set binding ""
        
        set cmd ::buli::BufferListBox
        set script [WrapCmdForKeyboard $w ::buli::BufferListBoxWindowName 1 $cmd]
        bind $SingleModBindtag <Control-Key-F12> $script

        set cmd ::tkcon::FocusConsole
        set script [WrapCmdForKeyboard $w ::ide_structure::BufferListBoxWindowName 1 $cmd]
        ::clcon_key::b bind $SingleModBindtag <Control-Key-period> $script
            # bind $w <Control-Key-Cyrillic_yu> $script

        set cmd ::edt::ShowSomeEditor
        set thatWindow [::ide_structure::EditorToplevelWindowName]
        set script [WrapCmdForKeyboard $w $thatWindow 0 $cmd]
        ::clcon_key::b bind $DoubleModBindtag <Control-Shift-E> $script
        # bind $w <Control-Shift-Key-Cyrillic_U> $script
        
        set thatWindow [::ide_structure::DebuggerToplevelWindowName]
        set cmd [list ::gui_util::FocusWindowByName $thatWindow]
        set script [WrapCmdForKeyboard $w $thatWindow 0 $cmd]
        ::clcon_key::b bind $DoubleModBindtag <Control-Shift-D> $script
        ::clcon_key::b bind $DoubleModBindtag <Control-Shift-t> ::inspthrd::ShowThreads
        # bind $DoubleModBindtag <Control-Shift-е> ::inspthrd::ShowThreads

        set thatWindow [::ide_structure::ErrorBrowserToplevelWindowName]
        set cmd [list ::gui_util::FocusWindowByName $thatWindow]
        set script [WrapCmdForKeyboard $w $thatWindow 0 $cmd]
        ::clcon_key::b bind $DoubleModBindtag <Control-Shift-R> $script    }

    # FIXME rename without "Dynamic" word. See recent and history menus for a sample or develop something new. 
    proc DynamicWindowMenu {w m args} {
        ## Window Menu
        ## Creates menu commands only. Keyboard bindings are created in ::window_menu::WindowMenuKeyBindings
        ## Which must be called explicitly when window is created
        ## w is a widget 
        ## m is a window submenu. It must be supplied by caller, e.g.:
        ## set m [menu [::tkcon::MenuButton $menu "7.Window" window]]
        ## SelfCommand is a command to activate current window. It must
        ## match command calculated by this function and is used to 
        ## identify current window and to disable it
        ## tl is a toplevel widget for "save window position" command
        ::named_args $args "-toplevel [winfo toplevel $w]"

        variable ::tkcon::PRIV

        ::gui_util::ClearMenu $m

        ## many vars are upvar'ed in the CalcEnabledForOneItem
        set SomeMenuDisabled 0
        set binding ""
        set state ""
        set w_top [winfo toplevel $w]
        
        set cmd ::buli::BufferListBox
        CalcEnabledForOneItem [::ide_structure::BufferListBoxWindowName] 1
	$m add command -label "Editor Buffer List" -accel "Control-F12" \
            -command $cmd -state $state
        
        set cmd ::tkcon::FocusConsole
        CalcEnabledForOneItem [::tkcon::ConsoleToplevelWindow] 0
	$m add command -label "REPL" -accel "Control-." \
            -command $cmd -state $state

        #
        set cmd ::edt::ShowSomeEditor
        CalcEnabledForOneItem [::edt::theTW] 0
        $m add command -label "Editor" -accel "Control-Shift-Р" \
            -command $cmd -state $state

        set thatWindow [::ide_structure::DebuggerToplevelWindowName]
        set cmd [list ::gui_util::FocusWindowByName $thatWindow]
        CalcEnabledForOneItem $thatWindow 0
        $m add command -label "Lisp Debugger" -accel "Control-Shift-d" \
            -command $cmd -state $state

        set thatWindow [::ide_structure::ErrorBrowserToplevelWindowName]
        set cmd [list ::gui_util::FocusWindowByName $thatWindow]
        CalcEnabledForOneItem $thatWindow 0
        $m add command -label "Compilation errors" -accel "Control-Shift-r" \
            -command $cmd -state $state

        $m add command -label "Threads" -accel "Control-Shift-t" \
            -command ::inspthrd::ShowThreads

        $m add separator
        $m add command -label "1. save window position" \
            -under 0  \
            -command "::win_lay::RecordLayout $(-toplevel)"
    }
    
}



## ::tkcon::HistoryMenu - dynamically build the menu for attached interpreters
##
# ARGS:	m	- menu widget
## KILL ME - i'm just a code sample
proc ::tkcon::HistoryMenu m {
    variable PRIV

    if {![winfo exists $m]} return
    set id [EvalSlave history nextid]
    if {$PRIV(histid)==$id} return
    set PRIV(histid) $id
    $m delete 0 end
    while {($id>1) && ($id>$PRIV(histid)-20) && \
	    ![catch {EvalSlave history event [incr id -1]} tmp]} {
	set lbl $tmp
	if {[string len $lbl]>60} { set lbl [string range $tmp 0 58]... }
	$m add command -label "$id: $lbl" -command "
	$::tkcon::PRIV(console) delete limit end
	$::tkcon::PRIV(console) insert limit [list $tmp]
	$::tkcon::PRIV(console) see end
	::tkcon::Eval $::tkcon::PRIV(console)"
    }
}


proc ::tkcon::ВставитьВМенюПунктыПроШрифты { m w Опр-функ } {
# Опр-функ имеет вид {{Виджет КодРазмера} ...Команды для установки размера...}

    set cmd [list apply ${Опр-функ} $w 0]
    $m add command -label "v. Small font" -command $cmd -under 0

    set cmd [list apply ${Опр-функ} $w 1]
    $m add command -label "c. Average font" -command $cmd -under 0

    set cmd [list apply ${Опр-функ} $w 2]
    $m add command -label "b. Big font" -command $cmd -under 0
}
