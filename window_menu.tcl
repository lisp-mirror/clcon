## clcon window menu

namespace eval ::window_menu {

    proc CanSwitchToTopLevel {from to CanCreateThatWindow} {
        if {$CanCreateThatWindow} {
            return 1
        } elseif {$from eq $to} {
            return 0
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
        upvar binding binding
        upvar cmd cmd
        upvar state state
        if {[CanSwitchToTopLevel $w_top \
               $windowToBeEnabled $CanCreateThatWindow]} {
            set state "normal"
            set binding $cmd
            # showVar w
            # showVar windowToBeEnabled
       } else {
            set SomeMenuDisabled 1
            set state "disabled"
            set binding ""
        }
    }
        

    proc DynamicWindowMenu {w SingleModBindtag DoubleModBindtag m} {
        ## Window Menu
        ## w is a widget to accept keyboard bindings, 
        ## m is a window submenu. It must be supplied by caller, e.g.:
        ## set m [menu [::tkcon::MenuButton $menu "7.Window" window]]
        ## SelfCommand is a command to activate current window. It must
        ## match command calculated by this function and is used to 
        ## identify current window and to disable it

        variable ::tkcon::PRIV

        ::gui_util::ClearMenu $m

        ## many vars are upvar'ed in the CalcEnabledForOneItem
        set SomeMenuDisabled 0
        set binding ""
        set state ""
        set w_top [winfo toplevel $w]
        
        set cmd ::buli::BufferListBox
        CalcEnabledForOneItem [::buli::BufferListBoxWindowName] 1

	$m add command -label "Buffer list" -accel "Control-F12" \
            -command $cmd -state $state
        bind $SingleModBindtag <Control-Key-F12> "$binding; break"
        
        set cmd ::tkcon::FocusConsole
        CalcEnabledForOneItem [::tkcon::ConsoleToplevelWindow] 0
	$m add command -label "Console" -accel "Control-." \
            -command $cmd -state $state
        ::tkcon::international_bind bind $SingleModBindtag <Control-Key-period> "$binding; break"
            # bind $w <Control-Key-Cyrillic_yu> $cmd
        #
        set cmd ::edt::ShowSomeEditor
        CalcEnabledForOneItem [::edt::theTW] 0
        $m add command -label "Editor" -accel "Control-Shift-e" \
            -command $cmd -state $state
        ::tkcon::international_bind bind $DoubleModBindtag <Control-Shift-E> "$binding; break"
        # bind $w <Control-Shift-Key-Cyrillic_U> $cmd

        set cmd [list ::gui_util::FocusWindowByName [::ldbg::DebuggerToplevelWindowName]]
        CalcEnabledForOneItem [::ldbg::DebuggerToplevelWindowName] 0
        $m add command -label "Debugger" -accel "Control-Shift-d" \
            -command $cmd -state $state
        ::tkcon::international_bind bind $DoubleModBindtag <Control-Shift-D> $binding
    }
    
}



## ::tkcon::HistoryMenu - dynamically build the menu for attached interpreters
##
# ARGS:	m	- menu widget
## KILL ME - i'm just a code sample
proc ::tkcon::HistoryMenu4 m {
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
