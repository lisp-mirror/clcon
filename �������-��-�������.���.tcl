## Окно для справки по символу. Копия с buffer-list.buli.tcl

namespace eval ::спс {

    # Правьмя - заменить на шрифт из консоли. 
    catch {font create tkconfixed -family Courier -size -20}

    # Creates grbr and returns it
    proc СоздатьЕщёОдноОкноПросмотровщикаСправки {  } {
        variable ::tkcon::PRIV

        set GrBrId [GenNamedCounter "СпрПоСимв"]
       
        set w [string cat $PRIV(base) ".СпрПоСимв_" $GrBrId]

        if {[winfo exists $w]} {
            error "Окно просмотровщика справки $w уже существует"
        }

        return $w
    }

    # Возвращает окно, см. также ::спс::УстановитьЗаголовок и ::спс::ТекстДляЗаписи
    proc СоздатьОкноСпрПоСимв { } {

        set w [СоздатьЕщёОдноОкноПросмотровщикаСправки]

        PrepareGui1 $w

        set tbl $w.tf.text

        ::win_lay::PositionATool $w

        DoGoToTop $w
        
        focus $tbl
        
        return $w
        
    }

    proc УстановитьЗаголовок { ОкноСпрПоСимв ИмяСимвола } {
       set w ${ОкноСпрПоСимв}
       if {[string length ${ИмяСимвола}] > 30} {
            wm title $w "спс: [string range ${ИмяСимвола} 0 26]... - $w"
       } else {
            wm title $w "спс: ${ИмяСимвола} - $w"
       }
    }

    proc ТекстДляЗаписи { ОкноСпрПоСимв } {
       return ${ОкноСпрПоСимв}.tf.text
    }

    proc DoGoToTop {w} {
        wm deiconify $w
        raise $w
    }

    proc ОкноСпрПоСимвFileMenu {w menu} {
         set m [menu [::tkcon::MenuButton $menu "1.Файл" file]]
         $m add command -label "1.Закрыть" -underline 0 -accel "Escape" -command [list destroy $w]
        bind $w <Escape>		[list destroy $w]
        ::clcon_key::b bind $w <Control-Key-w> [list destroy $w]
    }


    proc CellCmdForActiveCell {tbl Cmd} {
        CellCmd [$tbl index active] $Cmd
    }

    proc ОкноСпрПоСимвМенюПравка {w menu} {
        set m [menu [::tkcon::MenuButton $menu "2.Правка" buffer]]
        
        set команда [list ::tk_textCopy $w.tf.text]
        $m add command -label "Скопировать выделенное" -accel "Сtrl-С" -command ${команда}
        ::clcon_key::b bind $w <Control-Key-c> ${команда}
    }

    proc ОкноСпрПоСимвWindowMenu {w menu} {
        variable ::tkcon::COLOR
        set m [::tkcon::MenuButton $menu "7.Окно" window]
	menu $m -disabledforeground $COLOR(disabled) \
		-postcommand [list ::window_menu::DynamicWindowMenu $w $m]
        ::window_menu::WindowMenuKeyBindings $w $w $w
    }
    

    # Make toplevel widget and its children
    # Returns window
    proc PrepareGui1 {w} {

        set metrics [font measure tkconfixed "w"]
        toplevel $w -width [expr { 50 * $metrics }]
        wm withdraw $w
        
        # title 
        wm title $w "Заполняемое окно справки по символу $w"

        # ----------------------------------- menu -------------------
        
        set menu [menu $w.mbar]
        $w configure -menu $menu
        
        ОкноСпрПоСимвFileMenu $w $menu
        ОкноСпрПоСимвМенюПравка $w $menu
        ОкноСпрПоСимвWindowMenu $w $menu


        # --------------- frames, tablelist -----------------              
        ::gui_util::frame_clcon_text_and_scrollbars $w.tf {-readonly 1 -height 20}
        
        # -------------------------- layout -------------------
        
        set f1 $w.tf

        pack $f1 -fill both -expand 1

        return $w    
    }
}

