## ::tkcon::SwankInspect - opens a text for inspecting an object
## 
# Arguments:
#   LispExpr    lisp expression
# Returns:	nothing
##


# (:emacs-rex
#  (swank:inspect-nth-part 1)
#  "COMMON-LISP-USER" t 33)
# (:return
#  (:ok
#   (:title "#<symbol {DAA40AF}>" :id 0 :content
#           (("Its name is" ": "
#             (:value "\"A\"" 1)
#             "\n" "It is unbound." "\n" "It has no function value." "\n" "It is " "internal" " to the package: "
#             (:value "COMMON-LISP-USER" 2)
#             " "
#             (:action "[export]" 0)
#             " "
#             (:action "[unintern]" 1)
#             "\n" "Property list" ": "
#             (:value "nil" 3)
#             ...)
#            21 0 500)))

namespace eval ::insp {

    ## ::insp::InitInspector :
    # Example of dialog: 
    # (:emacs-rex
    #  (swank:init-inspector "*")
    #  "COMMON-LISP-USER" :repl-thread 32)
    # (:return
    #  (:ok
    #   (:title "#<cons {BD735A3}>" :id 0 :content
    #           (("A proper list:" "\n" "0" ": "
    #             (:value "a" 1)
    #             "\n" "1" ": "
    #             (:value "2" 2)
    #             "\n")
    #            10 0 500)))
    #  32)
    # We can't still eval in :repl-thread synchonously, so this is just a demo.
    # We will run init-inspector asynchronously and 
    proc InitInspector { LispExpr } {
        putd "Entered InitInspector with $LispExpr"
        set Quoted [::tkcon::QuoteLispObjToString $LispExpr]
        set RealExpr "(swank:init-inspector $Quoted)"
        ::tkcon::EvalInSwankAsync $RealExpr "::insp::SwankInspect1 \$EventAsList" :repl-thread 


        # (clco::convert-object-to-tcl '(:title "#<cons {BD735A3}>" :id 0 :content (("A proper list:" "\n" "0" ": "
        #              (:value "a" 1)
        #              "\n" "1" ": "
        #              (:value "2" 2)
        #              "\n")
        #             10 0 500)))
        
        #    set result "{l:title s#<cons\\ \\{BD735A3\\}> :id n0 :content {l{lsA\\ proper\\ list: sn s0 s:\\  {l:value sa n1 } sn s1 s:\\  {l:value s2 n2 } sn } n10 n0 n500 } } "
        #    SwankInspect1 $result             
    }

    # Initializes inspector with lisp expr. 
    proc SwankInspect { LispExpr } {
        # only passes request to emacs. Initialization is done asyncrhonously
        # by SwankInspect1
        InitInspector $LispExpr
    }

    proc NextPart {w begin end} {
        ::tkcon::EvalInSwankAsync "(swank:inspector-range $begin $end)" "::insp::NextPartCont $w \$EventAsList" :repl-thread     
    }

    proc NextPartCont { w EventAsList } {
        set InspectedContentU [::insp::ParseReturnOk $EventAsList]
        set InspectedData [::mprs::Unleash [lindex $InspectedContentU 0]]
        set InspectedMagicNumbers [lmap x [lrange $InspectedContentU 1 end] {::mprs::Unleash $x} ]
        putd $InspectedMagicNumbers
        set ObjectTooLarge [expr { [lindex $InspectedMagicNumbers 0] > [lindex $InspectedMagicNumbers 2] } ]

        # bind var for convenience
        set b [BodyTextOfInspector $w]
                
        foreach s $InspectedData {
            if {[::mprs::Consp $s] == 1} {
                set item [::mprs::Unleash $s]
                if {[lindex $item 0] eq {:value}} {
                    ::tkcon::WriteActiveText $b \
                        [::mprs::Unleash [lindex $item 1]] \
                        end \
                        "insp::InspectNthPart $w [::mprs::Unleash [lindex $item 2]]"
                } else {
                    $b RoInsert end "Я не знаю, что такое $s"
                }
            } else {
                $b RoInsert end [::mprs::Unleash $s]
            }
        }
        if { $ObjectTooLarge } {
#            set tag [::tkcon::UniqueTag $b]
#            $b tag configure $tag -foreground Red
#            $b RoInsert end {[ Object too large to inspect ]} $tag
             NextPart $w [lindex $InspectedMagicNumbers 2] [lindex $InspectedMagicNumbers 0]
        }

    }

    proc ParseReturnOk { EventAsList } {
        ::mprs::ParseReturnOk $EventAsList
    }
    
    proc InsertDataToShowOrBeep { w EventAsList } {
        # We well parse data here.
        set ReplyAsDict [::insp::ParseReturnOk $EventAsList]
        set HaveTitle [dict exists $ReplyAsDict :title]
        if { $HaveTitle } {
            set InspectedTitle [::mprs::Unleash [dict get $ReplyAsDict :title]]
            set InspectedContentU [::mprs::Unleash [dict get $ReplyAsDict :content]]
            set InspectedData [::mprs::Unleash [lindex $InspectedContentU 0]]
            set InspectedMagicNumbers [lmap x [lrange $InspectedContentU 1 end] {::mprs::Unleash $x} ]
            putd $InspectedMagicNumbers
            set ObjectTooLarge [expr { [lindex $InspectedMagicNumbers 0] > [lindex $InspectedMagicNumbers 2] } ]
        } else {
            set InspectedTitle "<no data>"
            set InspectedData ""
            set InspectedMagicNumbers ""
        }

        # bind var for convenience
        set b [BodyTextOfInspector $w]
        
        # clear old data if it existed
        [TitleOfInspector $w] RoDelete 1.0 end
        $b RoDelete 1.0 end
        
        # and now insert what we have parsed
        
        [TitleOfInspector $w] RoInsert 1.0 "$InspectedTitle\nВолшебные числа: $InspectedMagicNumbers"

        if { $ObjectTooLarge } {
#            set tag [::tkcon::UniqueTag $b]
#            $b tag configure $tag -foreground Red
#            $b RoInsert end {[ Object too large to inspect ]} $tag
        }
        foreach s $InspectedData {
            if {[::mprs::Consp $s] == 1} {
                set item [::mprs::Unleash $s]
                if {[lindex $item 0] eq {:value}} {
                    ::tkcon::WriteActiveText $b \
                        [::mprs::Unleash [lindex $item 1]] \
                        end \
                        "insp::InspectNthPart $w [::mprs::Unleash [lindex $item 2]]"
                } else {
                    $b RoInsert end "Я не знаю, что такое $s"
                }
            } else {
                $b RoInsert end [::mprs::Unleash $s]
            }
        }
        if { $ObjectTooLarge } {
             NextPart $w [lindex $InspectedMagicNumbers 2] [lindex $InspectedMagicNumbers 0]
        }
    }

    # This is a contiuation assigned on reply on initialization request 
    proc SwankInspect1 { EventAsList } {
        set w [PrepareGui1]
        InsertDataToShowOrBeep $w $EventAsList
        PrepareGui2 $w
    }

    proc SaveResult {} {
        ::tkcon::SendEventToSwank "(setf * (swank::istate.object swank::*istate*))" {}
    }

    ## insp::InspectNthPart
    # Args: Id
    # Returns: Don't matter
    # We will send message
    # (:emacs-rex
    #  (swank:inspect-nth-part 1)
    #  "COMMON-LISP-USER" t 35)
    # And arrange callback for it so that it asynchronously showed new contents in inspector
    proc InspectNthPart {w id} {
        set ContId [::tkcon::GenContinuationCounter]
        set OnReply "::insp::ShowSomethingNewInInspector $w \$EventAsList"
        ::tkcon::EvalInSwankAsync "(swank:inspect-nth-part $id)" $OnReply t $ContId
    }

    proc InspectorPop { w } {
        set ContId [::tkcon::GenContinuationCounter]
        set OnReply "::insp::ShowSomethingNewInInspector $w \$EventAsList"
        ::tkcon::EvalInSwankAsync "(swank:inspector-pop)" $OnReply t $ContId
    }

    proc InspectorNext { w } {
        set ContId [::tkcon::GenContinuationCounter]
        set OnReply "::insp::ShowSomethingNewInInspector $w \$EventAsList"
        ::tkcon::EvalInSwankAsync "(swank:inspector-next)" $OnReply t $ContId
    }


    proc InspectorReinspect { w } {
        set ContId [::tkcon::GenContinuationCounter]
        set OnReply "::insp::ShowSomethingNewInInspector $w \$EventAsList"
        ::tkcon::EvalInSwankAsync "(swank:inspector-reinspect)" $OnReply t $ContId
    }

    proc InspectorGotoSource { w } {
        set wForLisp [::tkcon::QuoteTclStringForLisp $w]

        ::tkcon::EvalInSwankAsync "(clco:inspector-goto-source $wForLisp)" {} t
    }

    proc ShowSomethingNewInInspector { w EventAsList } {
        InsertDataToShowOrBeep $w $EventAsList   
    }


    # Make toplevel widget and its children 
    proc PrepareGui1 {} {
        variable ::tkcon::PRIV
        # Create unique edit window toplevel
        set w $PRIV(base).__inspector
        set i 0
        while {[winfo exists $w[incr i]]} {}
        append w $i
        toplevel $w
        wm withdraw $w

        # title 
        set word "Инспектор"
        if {[string length $word] > 20} {
            wm title $w "[string range $word 0 16]... - tkcon Edit"
        } else {
            wm title $w "$word - tkcon Edit"
        }

        # --------------------------------- frames-----------------              
        
        # making title frame
        # height 2 - for magic numbers

        ::gui_util::frame_clcon_text_and_scrollbars $w.title {-height 2 -readonly 1}
        
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

        # ----------------------------------- menu -------------------
        
        set menu [menu $w.mbar]
        $w configure -menu $menu
        
        FileMenu $w $menu $w.body.text
        EditMenu $w $menu $w.body.text
        InspectMenu $w $menu $w.body.text
        WindowMenu $w $menu $w.body.text

        return $w
    }

    # layout and show inspector window
    proc PrepareGui2 {w} {    
        # --------------------------------- pack ---------------------
        # layout body elements in body 
        grid $w.body.text - $w.body.sy -sticky news
        grid $w.body.sx - -sticky ew
        grid columnconfigure $w.body 0 -weight 1 
        grid columnconfigure $w.body 1 -weight 1
        grid rowconfigure $w.body 0 -weight 1 

        # combine the entire widget
        pack $w.title -side top -fill x 
        pack $w.body -fill both -expand 1
        
        wm deiconify $w
        focus $w.body.text
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
        $m add separator

        $m add command -label "1. Результат в *" -under 0 \
            -command [list ::insp::SaveResult]
    }    


    proc InspectMenu {w menu text} {
        set m [menu [::tkcon::MenuButton $menu "3.Инспектор" inspect]]

        $m add command -label "Назад" -accelerator <BackSpace> -command [list ::insp::InspectorPop $w]
        bind $w <BackSpace> [list ::insp::InspectorPop $w]
        bind $w <Alt-Key-Left> [list ::insp::InspectorPop $w]

        $m add command -label "Вперёд" -accelerator <Alt-Key-Right> -command [list ::insp::InspectorNext $w]
        bind $w <Alt-Key-Right> [list ::insp::InspectorNext $w]

        $m add command -label "0. Перейти к исходному тексту" -under 0 -command [list ::insp::InspectorGotoSource $w]

        $m add command -label "Обновить" -accelerator <F5> -command [list ::insp::InspectorReinspect $w]
        bind $w <F5> [list ::insp::InspectorReinspect $w]
        
    }    

    # text is ignored
    proc WindowMenu {w menu text} {
        variable ::tkcon::COLOR
        set m [::tkcon::MenuButton $menu "7.Окно" window]

	menu $m -disabledforeground $COLOR(disabled) \
		-postcommand [list ::window_menu::DynamicWindowMenu $w $m]

        ::window_menu::WindowMenuKeyBindings $w $w $w
    }


    proc TitleOfInspector {w} {
        return $w.title.text
    }

    proc BodyTextOfInspector {w} {
        return $w.body.text
    }

    proc DebugStartup {} {
        variable ::tkcon::OPT
        if { $OPT(putd-enabled) } {
            ::insp::InitInspector {'defun}
        }
    }

}

#::insp::DebugStartup
