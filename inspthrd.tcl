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
        if {[lsearch -exact [list reader-thread control-thread {Swank Sentinel}] $threadName] != -1 \
             || $threadName eq "Swank $OPT(swank-port)"} {
            return 1
        } else {
            return 0
        }
    }
    

    proc PrepareGui1 {Reply} {
        variable ::tkcon::PRIV
        # Create unique edit window toplevel
        set w $PRIV(base).__inspthreads
        set i 0
        while {[winfo exists $w[incr i]]} {}
        append w $i
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
                $b RoInsert end [::mprs::Unleash [lindex $us 0]]
                $b RoInsert end "\t" 
                $b RoInsert end $threadName
                $b RoInsert end "\t\t\t\t\t" 
                $b RoInsert end [::mprs::Unleash [lindex $us 2]]
                $b RoInsert end "\n" 
            } else {
                $b RoInsert end [::mprs::Unleash [lindex $us 0]]
                $b RoInsert end "\t" 
                if {[ОпасноеИмяПотока $threadName]} {
                    set ZnakOpasnosti "⚠" 
                } else {
                    set ZnakOpasnosti " "
                }
                $b RoInsert end $ZnakOpasnosti
                ::tkcon::WriteActiveText $b \
                     $threadName \
                     end \
                     [list inspthrd::BreakNthThread $n]
                $b RoInsert end "\t\t\t\t\t" 
                $b RoInsert end [::mprs::Unleash [lindex $us 2]]
                $b RoInsert end "\n" 
            }
            set n [expr $n + 1]
        }
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
        wm deiconify $w
        focus $w.body.text
        return $w
    }

    proc BreakNthThread {n} {
        ::tkcon::SendEventToSwank "(swank:debug-nth-thread $n)" {} 0
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