## editor - part which is independent on window management.

namespace eval ::edt {
    proc CanonicalizeFileName {name} {
        global tcl_platform
        if {$tcl_platform(platform) == "windows"} {
            set name [::WindowsFileNameToUnix $name]
            return [string tolower $name]
        } else {
            return $name
        }
    }

    # This function increases ReuseCounter for non-reusable parameter sets
    # Take care to call it once for every parameter set.
    proc CanonicalizeEditArgs {word opts} {
        set type [dict get $opts -type]
        if {$type eq "file"} {
            set word [CanonicalizeFileName $word]
            return [list $word -type $type]
        } elseif {$type eq "newfile"} {
            set word "untitled [::edt::GenReuseCounter]"
            return [list $word -type $type]
        } elseif {$type eq "error"} {
            return [list tcl_error -type $type -no [GenReuseCounter]]
        } else {
            # never reuse procs, vars, errors
            return [list $word -type $type -no [GenReuseCounter]]
        }
    }

    # Parses line and returns list of {word opts tail}
    proc EditorParseArgs {args} {
        variable ::tkcon::PRIV
        variable ::tkcon::COLOR
        variable ::tkcon::OPT

        set args [lindex $args 0]
        
        set opts [dict create -find {} -type {} -attach {} -wrap {char} ]             
        while {[string match -* [lindex $args 0]]} {
            switch -glob -- [lindex $args 0] {
                -f*	{ dict set opts -find [lindex $args 1] }
                -a*	{ dict set opts -attach [lindex $args 1] }
                -t*	{ dict set opts -type [lindex $args 1] }
                -w*	{ dict set opts -wrap [lindex $args 1] }
                -o*     { dict set opts -offset [lindex $args 1] }
                -byteoffset { dict set opts -byteoffset [lindex $args 1] }
                --	{ set args [lreplace $args 0 0]; break }
                default {return -code error "unknown option \"[lindex $args 0]\""}
            }
            set args [lreplace $args 0 1]
        }

        # determine what interpreter we are dealing with (broken)
        # if {[llength [ dict get $opts -attach]]} {
        #     foreach {app type} [ dict get $opts -attach] {break}
        # } else {
        #     foreach {app type} [tkcon attach] {break}
        # }
        
        foreach {app type} [tkcon attach] {break}
        
        set word [lindex $args 0]

        if {[dict get $opts -type] == {}} {
            if {[llength [::tkcon::EvalOther $app $type info commands [list $word]]]} {
                dict set opts -type "proc"
            } elseif {[llength [::tkcon::EvalOther $app $type info vars [list $word]]]} {
                dict set opts -type "var"
            } elseif {[::tkcon::EvalOther $app $type file isfile [list $word]]} {
                dict set opts -type "file"
            }
        }
        if {[dict get $opts -type] == {}} {
            return -code error "unrecognized type '$word'"
        }

        set tail $args

        return [list $word $opts $tail]
    }

    proc CompileAndLoadTheFile {Bi} {
        set clcon_text [Bi2btext $Bi]
        set opened_file [$clcon_text cget -opened_file]
        set FileName [$opened_file cget -filename]
        set qFileName [::tkcon::QuoteLispObjToString $FileName]
        ::edt::Save $Bi $clcon_text 0
        set form "(clco::compile-file-for-tcl \"$clcon_text\" $qFileName)"
        ::tkcon::FocusConsole
        ::tkcon::SendEventToSwank $form {} 1 t
    }

    proc УстановитьЭтотПакетВКонсоли {text} {
        ::clcon_text::CallOduvanchikFunction $text "odu::УСТАНОВИТЬ-ЭТОТ-ПАКЕТ-В-КОНСОЛИ-COMMAND nil" {}
    }
    
    proc КомпилироватьТекущееОпределение {text} {
        ::clcon_text::CallOduvanchikFunction $text "odu::compile-current-defun-command nil" {}
    }


    proc CompileSystem {text} {
        set console [::tkcon::CurrentConsole]
        set w [$text RealText]

        ::tkcon::ПоложитьТекущуюПозициюНаPosStack $w
        
        ::clcon_text::CallOduvanchikFunction $text "odu::compile-system-command nil" {}
    }

    proc ТестироватьСистемуASDF {text} {
        set console [::tkcon::CurrentConsole]
        set w [$text RealText]

        ::tkcon::ПоложитьТекущуюПозициюНаPosStack $w
        
        ::clcon_text::CallOduvanchikFunction $text "odu::test-system-command nil" {}
    }

    # Note we do not save file and 
    proc FindCurrentFileDeclarations {clcon_text} {
        set opened_file [$clcon_text cget -opened_file]
        set FileName [$opened_file cget -filename]
        set qFileName [::tkcon::QuoteLispObjToString $FileName]
        set form "(clco::find-current-file-declarations $qFileName)"
        ::tkcon::SendEventToSwank $form {} 1 t
    }



    proc EncodeTypeForBufferList {type} {
        switch -exact $type {
            file {return "f"}
            proc {return "p"}
            var {return "v"}
            error {return "e"}
            default {return "?"}
        }
    }

    proc ПоискаСвязейСимволаПродолжение {clcon_text EventAsList ИмяКоманды} {
        set Head [::mprs::Unleash [lindex $EventAsList 0]]
        ::mprs::AssertEq $Head ":return"
        set l2 [::mprs::Unleash [lindex $EventAsList 1]]
        set h2 [::mprs::Unleash [lindex $l2 0]]
        if {$h2 eq ":ok"} {
            set code [::mprs::Unleash [lindex $l2 1]]
            set proc [subst -nocommand {{w} {$code}}]
            # tk_messageBox -message $proc
            apply $proc [::tkcon::CurrentConsole]
        } else {
            tk_messageBox -parent $clcon_text -message "Команда ${ИмяКоманды} завершилась ненормально"
        }
        $clcon_text Unfreeze
          
    }

    proc SkopirovatqIdentVBuferObmenaProdolzhenie {clcon_text EventAsList} {
        set Head [::mprs::Unleash [lindex $EventAsList 0]]
        ::mprs::AssertEq $Head ":return"
        set l2 [::mprs::Unleash [lindex $EventAsList 1]]
        set h2 [::mprs::Unleash [lindex $l2 0]]
        if {$h2 eq ":ok"} {
            set code [::mprs::Unleash [lindex $l2 1]]
            # tk_messageBox -message $code
            clipboard clear
            clipboard append $code 
        } else {
            tk_messageBox -parent $clcon_text -message "Команда SkopirovatqIdentVBuferObmena завершилась ненормально"
        }
        $clcon_text Unfreeze
          
    }


    # See also ::tkcon::LispFindDefinition
    # У этой функции есть копипасты
    proc FindSourceCommand {text} {
        set console [::tkcon::CurrentConsole]
        set w [$text RealText]

        ::tkcon::ПоложитьТекущуюПозициюНаPosStack $w
        
        ::clcon_text::CallOduvanchikFunction $text "odu::find-source-command nil" {{
            ::edt::ПоискаСвязейСимволаПродолжение $clcon_text $EventAsList "FindSource"
        }} {send_selection 1}
    }

    # Копипаст из FindSourceCommand
    proc КтоВызываетФункцию {text} {
        set console [::tkcon::CurrentConsole]
        set w [$text RealText]

        ::tkcon::ПоложитьТекущуюПозициюНаPosStack $w
        
        ::clcon_text::CallOduvanchikFunction $text "odu::kto-vyyzyyvaet-funkciyu-command nil" {{
            ::edt::ПоискаСвязейСимволаПродолжение $clcon_text $EventAsList "КтоВызываетФункцию"
        }} {send_selection 1}
    }


    # Копипаст из FindSourceCommand
    proc ЯрНайтиИсходникCommand {text} {
        set console [::tkcon::CurrentConsole]
        set w [$text RealText]

        ::tkcon::ПоложитьТекущуюПозициюНаPosStack $w
        
        ::clcon_text::CallOduvanchikFunction $text "odu::yar-nayiti-iskhodnik-command nil" {{
            ::edt::ПоискаСвязейСимволаПродолжение $clcon_text $EventAsList "FindSource"
        }} {send_selection 1}
    }


    proc ЯрНайтиСгенерированныйТекстCommand {text} {
        set console [::tkcon::CurrentConsole]
        set w [$text RealText]

        ::tkcon::ПоложитьТекущуюПозициюНаPosStack $w
        
        ::clcon_text::CallOduvanchikFunction $text "odu::nayiti-iskhodnik-zpt-porozhdyonnyyyi-iz-ehtogo-mesta-command nil" {{
            ::edt::ПоискаСвязейСимволаПродолжение $clcon_text $EventAsList "::edt::ЯрНайтиСгенерированныйТекстCommand"
        }} {send_selection 1}
    }


    proc FindPackage {text} {
        set console [::tkcon::CurrentConsole]
        set w [$text RealText]

        ::tkcon::ПоложитьТекущуюПозициюНаPosStack $w
        
        ::clcon_text::CallOduvanchikFunction $text "odu::find-package-command nil" {{
            ::edt::ПоискаСвязейСимволаПродолжение $clcon_text $EventAsList "Перейти к определению текущего пакета"
        }}
    }

    proc FindSystem {text} {
        # Удали это set console [::tkcon::CurrentConsole]
        set w [$text RealText]

        ::tkcon::ПоложитьТекущуюПозициюНаPosStack $w
        
        ::clcon_text::CallOduvanchikFunction $text "odu::find-system-command nil" {{
            ::edt::ПоискаСвязейСимволаПродолжение $clcon_text $EventAsList "Перейти к определению текущей системы ASDF"
        }}
    }

    proc udalitq-fajjly-rezulqtata-sborki-sistemy {text} {
        ::clcon_text::CallOduvanchikFunction $text "odu::udalitq-fajjly-rezulqtata-sborki-sistemy-command nil" {{
            $clcon_text Unfreeze
            ::tkcon::FocusConsole

        }}
    }

    # See also ::tkcon::LispFindDefinition
    proc SkopirovatqIdentVBuferObmena {text} {
        set console [::tkcon::CurrentConsole]
        ::clcon_text::CallOduvanchikFunction $text "odu::find-symbol-command nil" {{
            ::edt::SkopirovatqIdentVBuferObmenaProdolzhenie $clcon_text $EventAsList
        }} {send_selection 1}
    }    

    proc EditFileNameUnderCursorCommand {btext} {
        set console [::tkcon::CurrentConsole]
        ::tkcon::ПоложитьТекущуюПозициюНаPosStack [$btext RealText]
        ::clcon_text::CallOduvanchikFunction $btext "odu::edit-file-name-under-cursor-command nil" {{
            ::edt::EditFileNameUnderCursorContinuation $clcon_text $EventAsList
        }}
    }
        
    proc EditFileNameUnderCursorContinuation {clcon_text EventAsList} {
        set V1 [ ::mprs::ParseReturnOk $EventAsList ]
        set Priznak [ ::mprs::Unleash [ lindex $V1 0 ]]
        if {$Priznak == 0} {
            puts "Нет имени файла под курсором"
        } else {
            set FileName [::mprs::Unleash [lindex $V1 1]]
            if {$Priznak == 1} {
                puts "Файл не существует. Для создания подайте команду .ред-файл $FileName"
            } else {
                ::edt::edit -type file -- $FileName
            }
        }
        $clcon_text Unfreeze
    }


    # See also ::tkcon::LispHyperdocLookup
    proc LispHyperdocLookupCommand {text} {
        set console [::tkcon::CurrentConsole]
        ::clcon_text::CallOduvanchikFunction $text "odu::hyperdoc-lookup-command nil" "" {send_selection 1}
    }    

    proc LispDescribeAllCommand {text} {
        set w [ ::спс::СоздатьОкноСпрПоСимв ]
        ::clcon_text::CallOduvanchikFunction $text "odu::describe-all-command nil" [subst -nocommands {{
            ::edt::LispDescribeAllContinuation \$clcon_text \$EventAsList $w
        }}] {send_selection 1}
    }

    proc OpenUrl {text} {
        ::tkcon::EvalInSwankAsync "(clco::open-url \"$text\")" {} t
    }

    # надо бы обобщить - или clcon_text, или консоль. Проверять по winfo class 
    # как-то так:
    #510 CL-USER>..winfo class [ ::tkcon::CurrentConsole ]
    #Text
    #513 CL-USER>..winfo class [ ::edt::c_btext ]
    #Btext
    # Тогда можно будет привязать эту команду к консоли.

    proc LispDescribeAllContinuation {clcon_text EventAsList ОкноСпрПоСимв} {
        set V1 [ ::mprs::ParseReturnOk $EventAsList ]
        set text [ ::mprs::Unleash [ lindex $V1 0 ]]
        $clcon_text Unfreeze
        set b [::спс::ТекстДляЗаписи ${ОкноСпрПоСимв}]
        ::спс::УстановитьЗаголовок ${ОкноСпрПоСимв} $text
        if {[llength $V1] == 1} {
            $b RoInsert end "$text - это не символ"
        } else {
            set argSwank [ ::mprs::Unleash [ lindex $V1 1 ]]
            set Links [ ::mprs::Unleash [ lindex $V1 2 ]]
            set Describe [ ::mprs::Unleash [ lindex $V1 3 ]]
            set Defuns [ ::mprs::Unleash [ lindex $V1 4 ]]
            $b RoInsert end "Символ: $text (далее вывод в его пакете)\n"
            $b RoInsert end "Параметры: "
            $b RoInsert end $argSwank
            $b RoInsert end "\n"
            $b RoInsert end "Документация:\n"
            if {$Links ne "COMMON-LISP:NIL" } {
                $b RoInsert end "CLTL2: "
                foreach Link $Links {
                    eval [ ::mprs::Unleash $Link ]
                    $b RoInsert end "\n"
                }
            }
            ::tkcon::WriteActiveText $b "Hyperdoc" end "::edt::LispHyperdocLookupCommand $clcon_text" error
            $b RoInsert end "\n\n"
            $b RoInsert end "Описание: \n"
            $b RoInsert end $Describe 
            $b RoInsert end "\nОпределение: \n"
            puts $Defuns
            eval $Defuns
        }
    }
    
    
    proc ReadFileIntoString {word {RemoveLastNewline 0}} {
        set obj [string cat "__tkcon" [GenNamedCounter "ReadFileObj"]]
        if {$RemoveLastNewline} {
            set NoNewLineOption "-nonewline"
        } else {
            set NoNewLineOption ""
        }
        set cmd [subst -nocommands {
            set ${obj}(fid) [open {$word} r]
            set ${obj}(data) [read $NoNewLineOption \$${obj}(fid)]
            close \$${obj}(fid)
            after 1000 unset ${obj}
            return \$${obj}(data)
        }
                ]
        set line [::tkcon::EvalOther {} slave eval $cmd]
        return $line
        # if {$RemoveLastNewline && [string range $line end end] eq "\n"} {
        #     puts "oKi"
        #     return [string range $line 0 end-1]
        # } else {
        #     return $line
        # }
    }

    # This will be an option
    # If true, we allow for only one editor window at a time, joungling frames in it
    # New window to the same place where old one was
    proc SingleEditorWindow {} {
        return 1
    }

    # Returns list of two indices
    proc TextSelectionCoordinates {text} {
        $text tag nextrange sel 1.0 end
    }

    # This should go to special file?
    proc TextSetSelectionTo {text from to} {
        $text tag remove sel 1.0 end
        $text tag add sel $from $to
    }

    proc TextRemoveSelection {text} {
        $text tag remove sel 1.0 end
    }

    proc e_indent {text {extra "    "}} {
        set w $text
        set lineno [expr {int([$w index insert])}]
        set line [$w get $lineno.0 $lineno.end]
        regexp {^(\s*)} $line -> prefix
        if {[string index $line end] eq "\{"} {
            tk::TextInsert $w "\n$prefix$extra"
        } elseif {[string index $line end] eq "\}"} {
            if {[regexp {^\s+\}} $line]} {
                $w delete insert-[expr [string length $extra]+1]c insert-1c
                tk::TextInsert $w "\n[string range $prefix 0 end-[string length $extra]]"
            } else {
                tk::TextInsert $w "\n$prefix"
            }
        } else {
            tk::TextInsert $w "\n$prefix"
        }
    }

    proc EnsureToplevelWindowWithPathname {tw} {
        if {![winfo exists $tw]} {
            toplevel $tw
            wm withdraw $tw
        }
        return $tw
    }

    # Removes old editor bindtags and set new ones
    # New bindtags are DoubleMod$suffix, SingleMod$suffix, NoMod$suffix
    proc SetEditorBindtags {path suffix} {
        set s $suffix
        set result [list DoubleMod$s SingleMod$s NoMod$s]
        foreach bindtag [bindtags $path] {
            switch -glob -- $bindtag {
                DoubleMod* -
                SingleMod* -
                NoMod* {
                    # do nothing
                }
                default {
                    set result [linsert $result end $bindtag]
                }
            }
        }
        bindtags $path $result
    }

    proc cMenuBar {{SubMenuSuffix {}}} {
        string cat [theTW] ".mbar" $SubMenuSuffix
    }

    # Initialization of editor GUI, buffer-independent part.
    # Called only once when we create first editor window.
    # So if you change it, restart clcon 
    proc SetupEditorWindowCommon {tw} {
        variable ::tkcon::PRIV
        variable ::tkcon::COLOR
        variable ::tkcon::OPT

        wm protocol $tw WM_DELETE_WINDOW [list tk_messageBox -parent $tw -title "Попытка закрыть редактор" -message "Чтобы закрыть редактора, закройте все окна редактора или выйдите из clcon"]

        set notebook [theNotebook]
        ttk::notebook $notebook
        pack $notebook -fill both -expand 1
        bind $notebook <<NotebookTabChanged>> "::edt::SwitchToThisTab %d"
        ttk::notebook::enableTraversal $notebook 

        set menu [menu [cMenuBar]]
        $tw configure -menu $menu

        # Make menu bar
        menu [::tkcon::MenuButton $menu "1.Файл" файл]
        menu [::tkcon::MenuButton $menu "2.Правка" правка]
        menu [::tkcon::MenuButton $menu "3.Лисп" lisp]
        menu [::tkcon::MenuButton $menu "4.Tcl/Md" tcl]
        menu [::tkcon::MenuButton $menu "7.Окно" окно]        
        menu [::tkcon::MenuButton $menu "8.Секрет" secret]
        
    }
    
}
