## clcon project - IDE commands
## (c) Denis Budyak 2015
## MIT License

namespace eval clconcmd {
    proc help {} {
    puts "Best way to find a help is to watch a source - it contains comments. To find source, type in command's name with ::clconcmd:: prefix at the console (you can use completion) and then use tcl find source.
Currently defined commands are: "
    set result1 [info procs ::clconcmd::*]
    regsub -all "::clconcmd::" $result1 "" result2
    puts $result2
    }
    
    #proc history {} {
    #    history
    #}

    # Load filename, name is relative to clcon source directory,
    # Name should be in unix style (/ instead of \)
    proc tcsoh {filename} {
        tkcon main TkconSourceHere $filename
    }

    # Show history. If searchstring is given, only shows items containing 
    # searchstring as a case insensitive glob pattern. 
    proc hist {{searchstring ""}} {
        if {$searchstring ne ""} {
            foreach c [split [tkcon main history] \n] {
                if {[string match -nocase [string cat * $searchstring *] $c]} {
                    puts $c
                }
            }
        } else {
            tkcon main history
        }
    }
    
    proc ист {args} {
        hist {*}$args
    }    
        
    # Run oduvanchik command (with 
    proc o {commandNameWoPrefix} {
        ::edt::oImplementation $commandNameWoPrefix
    }
    
    proc о {имяКомандыБезПрефикса} { o ${имяКомандыБезПрефикса}}

    # Swank inspect star
    proc insp* {} {
        tkcon main ::insp::SwankInspect "*"
    }

    proc инсп* {} {
        insp*
    }

    # 
    proc edit {filename} {
        ::edt::edit -type file -wrap char -- $filename
    }

    proc ред-файл {filename} {
        edit $filename
    }

    # Tcl apropos
    proc tapr {str} {
        foreach x [::tkcon::TclApropos ${str}] {
            puts ${x}
        }
    }

    proc тапр {str} { tapr $str }

    # Lisp apropos 
    proc apr {str} {
        set qStr [::tkcon::QuoteLispObjToString $str]
        set form "(cl:apropos $qStr)" 
        ::tkcon::FocusConsole
        ::tkcon::SendEventToSwank $form {puts ""} 1 t
    }
  
    proc апр {str} { apr $str }        

    # Find in clcon sources 
    proc fics {str} {
        set qStr [::tkcon::QuoteLispObjToString $str]
        set form "(clco::find-in-clcon-sources $qStr)" 
        ::tkcon::FocusConsole
        ::tkcon::SendEventToSwank $form {puts ""} 1 t
    }
 
    # искать в исходниках яра
    proc иия { str } { fics $args }

    # 
    proc finf {args} {
        # Find in files. Synopsys ([[]] means optional part)
        # .finf [[ -types list_of_types ]] dirs string
        # default list of types is {asd lisp}
        set keys [lrange $args 0 end-2]
        set dirs [lindex $args end-1]
        set string [lindex $args end]
        named_args $keys {-types "asd lisp"}
        ::clconcmd_inner::finf_inner $dirs $(-types) $string
    }

    proc иф { args } { 
        set keys [lrange $args 0 end-2]
        set dirs [lindex $args end-1]
        set string [lindex $args end]
        named_args $keys {-типы "asd lisp"}
        ::clconcmd_inner::finf_inner $dirs $(-типы) $string
    }

    proc edit_initialization_file {} {
        variable ::tkcon::PRIV
        edit $::tkcon::PRIV(rcfile)
    }

    proc редактировать_файл_иницаилизации {} { edit_initialization_file }
}
    
