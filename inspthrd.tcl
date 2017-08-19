namespace eval ::inspthrd {

    proc ShowThreads {} {
        ::tkcon::EvalInSwankAsync "(swank:list-threads)" "::inspthrd::ShowThreadsCont \$EventAsList" t                        
    }

    proc ShowThreadsCont {EventAsList} {
        set Reply [::mprs::ParseReturnOk $EventAsList]
        PrepareGui1 $Reply
    }

    # Имя потока, отладка которого нас убъёт
    proc ОпасноеИмяПотока {threadName} {
        variable ::tkcon::OPT
        set OpasnoKonsolb 0 
        set OpasnoGPI 0 

        if {$threadName eq "main thread"} {
            set OpasnoKonsolb 1
        }

        if {[lsearch -exact [list reader-thread control-thread {Swank Sentinel}] $threadName] != -1 \
             || $threadName eq "Swank $OPT(swank-port)"} {
            set OpasnoGPI 1
        }
     
        return [list $OpasnoKonsolb $OpasnoGPI]
        
    }
    

    proc PrepareGui1 {Reply} {
        variable ::tkcon::PRIV
        variable ::tkcon::OPT
        # Create unique edit window toplevel
        set w $PRIV(base).threadList
        # set i 0
        if {[winfo exists $w]} {
            destroy $w
        }
        toplevel $w
        wm withdraw $w

        # title 
        set word "Список потоков"
        if {[string length $word] > 20} {
            wm title $w "[string range $word 0 16]... - tkcon Edit"
        } else {
            wm title $w "$word - tkcon Edit"
        }

        # make body frame    
        frame $w.body
        ::clcon_text::clcon_text $w.body.text -readonly 1

        ::gui_util::ConfigureTextFonts $w.body.text
        # override with small font for temporary
        $w.body.text configure \
            -xscrollcommand [list $w.body.sx set] \
            -yscrollcommand [list $w.body.sy set] 
        catch {
            # 8.5+ stuff
            set tabsp [expr {$OPT(tabspace) * [font measure $OPT(font) 0]}]
            $w.body.text configure -tabs [list $tabsp left] -tabstyle wordprocessor
        }

        scrollbar $w.body.sx -orient h -command [list $w.body.text xview]
        scrollbar $w.body.sy -orient v -command [list $w.body.text yview]

#        tk_messageBox -message [::mprs::Unleash [lindex $Reply 0]]
        set b $w.body.text
        set n -1
        $b RoInsert end "Щёлкните по потоку для его отладки\n"
        foreach s $Reply {
            set us [::mprs::Unleash $s]
            set threadName [::mprs::Unleash [lindex $us 1]]
            if {$n == -1} {
                $b RoInsert end "№\tСтатус\tИмя\t\t\tОтладить в консоли SBCL\t\tОтл.в ГПИ\n"
            } else {
                $b RoInsert end [::mprs::Unleash [lindex $us 0]]
                $b RoInsert end "\t" 
                $b RoInsert end [::mprs::Unleash [lindex $us 2]]
                $b RoInsert end "\t" 
                $b RoInsert end $threadName
                $b RoInsert end "\t\t\t\t\t" 
                foreach {OpasnoKonsolb OpasnoGPI} [ОпасноеИмяПотока $threadName] {}
                if {$OpasnoKonsolb} { set ZK "⚠" } else { set ZK " " }
                if {$OpasnoGPI} { set ZG "⚠" } else { set ZG " " }             
                ::tkcon::WriteActiveText $b [string cat $ZK "жми"] end [list ::inspthrd::BreakNthThreadInBlackConsole $n]
                $b RoInsert end "\t" 
                ::tkcon::WriteActiveText $b [string cat $ZG "жми"] end [list ::inspthrd::BreakNthThread $n]
                $b RoInsert end "\n" 
            }
            set n [expr $n + 1]
        }
        $b RoInsert end "\nДля отладки в консоли SBCL нужно после появления сообщения со списком перезапусков (restart-ов) много раз нажать Enter, пока не появится подсказка отладчика. Если вы уже отлаживаете один поток в консоли, второй не пытайтесь начинать отлаживать"

        # ::ro_out::I $w end $Reply

        # ----------------------------------- menu -------------------
        
        set menu [menu $w.mbar]
        $w configure -menu $menu
        
        FileMenu $w $menu $w.body.text
        EditMenu $w $menu $w.body.text
#        InspectMenu $w $menu $w.body.text
        WindowMenu $w $menu $w.body.text

        grid $w.body.text - $w.body.sy -sticky news
        grid $w.body.sx - -sticky ew
        grid columnconfigure $w.body 0 -weight 1 
        grid columnconfigure $w.body 1 -weight 1
        grid rowconfigure $w.body 0 -weight 1 
        pack $w.body -fill both -expand 1
        ::win_lay::PositionATool $w
        wm deiconify $w
        focus $w.body.text
        return $w
    }

    proc BreakNthThread {n} {
        ::tkcon::EvalInSwankAsync "(swank:debug-nth-thread $n)" {} t
    }
    
    proc BreakNthThreadInBlackConsole {n} {
        ::tkcon::EvalInSwankAsync "(clco:otladitq--n--jj-potok-v-chjornojj-konsoli $n)" {} t
    }

    proc FileMenu {w menu text} {
        set m [menu [::tkcon::MenuButton $menu "1.Файл" file]]
        $m add command -label "Сохранить как..."  -underline 0 \
            -command [list ::tkcon::Save {} widget $text]
        $m add command -label "Добавить к..."  -underline 0 \
            -command [list ::tkcon::Save {} widget $text a+]
        $m add separator
        $m add command -label "Закрыть" -underline 0 -accel "Control-w" \
            -command [list destroy $w]
        ::clcon_key::b bind $w <Control-Key-w>		[list destroy $w]
    }

    proc EditMenu {w menu text} {
        set m [menu [::tkcon::MenuButton $menu "2.Правка" edit]]
        $m add command -label "Копировать"  \
            -command [list tk_textCopy $text]
        $m add separator

        $m add command -label "Найти" \
            -command [list ::fndrpl::OpenFindBox $text "text" "find" {}]
        ::clcon_key::b bind $w <Control-Key-f>             [list ::tkcon::Findbox $text]
    }    


    # text is ignored
    proc WindowMenu {w menu text} {
        variable ::tkcon::COLOR
        set m [::tkcon::MenuButton $menu "7.Окно" window]

	menu $m -disabledforeground $COLOR(disabled) \
		-postcommand [list ::window_menu::DynamicWindowMenu $w $m]

        ::window_menu::WindowMenuKeyBindings $w $w $w
    }
}