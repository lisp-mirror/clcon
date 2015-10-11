
## A ls common lisp's defvar, http://www.lispworks.com/documentation/lw60/CLHS/Body/m_defpar.htm
proc defvar {name value} {
    if {$value eq ""} {
        set rhs_code "{}"
    } else {
        set rhs_code $value
    }
    set code [subst -nocommands {
        uplevel 1 {
            variable $name
            if {![info exists $name]} {
                set $name $rhs_code
            }
        }
    }]
    eval $code
}

namespace eval ::record_definition {
    # Definition locations database
    defvar db [dict create]
    defvar proc_was_renamed 0
}

proc ::record_definition::dump_all_infos {} {
    for {set x 0} {$x<100} {incr x} { 
        catch { set frame$x [ info frame $x ] }
        if {[info exists frame$x]} {
            showVar frame$x
        }
    }
}

proc ::__record_definition_myproc {name arglist body} {
    variable ::record_definition::db
    set level [expr { [ info frame ] - 1 }]
    # dump_all_infos
    set fi [ info frame $level ] 
    set ns [uplevel { namespace current }]
    if {$ns ne ""} {
        set QualifiedName [string cat $ns "::" $name]
    } else {
        set QualifiedName $name
    }
    # showVar ns
    if {[dict exists $fi file]} {
        # showVar fi
        dict set db $QualifiedName \
            [dict create file [dict get $fi file] line [dict get $fi line]]
    } else {
        dict unset db $QualifiedName
    }
    set code1 [list ::__hidden_real_proc $name $arglist $body ]
    set code [list uplevel 1 $code1]
    #if {$ns ne ""} {
    #    set code [list namespace eval $ns $code1 ]
    #} else {
    #    set code $code1
    #}
    # showVar code
    eval $code
    return {}
}

proc ::record_definition::EditProcedure {QualifiedName} {
    variable db
    set location_info [dict get $db $QualifiedName]
    set li $location_info
    if {$li ne {}} {
        ::tkcon::EditFileAtLine [dict get $li file] [dict get $li line]
    } else {
        ::edt::edit -type proc -- $QualifiedName
    }
}

# Modifies proc no more than once
proc ::record_definition::AlterProc {} {
    variable proc_was_renamed
    showVar proc_was_renamed
    if {$proc_was_renamed} {
        puts "proc was already altered by record_definition::AlterProc"
    } else {
        rename ::proc ::__hidden_real_proc
        rename ::__record_definition_myproc ::proc
        set proc_was_renamed 1
    }
}

::record_definition::AlterProc
    
#::record_definition::myproc bbb {x} {
#  showVar x
#

#namespace eval tkcon {
#  ::record_definition::myproc ccc {x} {
#    variable PRIV
#    puts $PRIV(console)   
#    error "Bul'k"
#  }
#}

catch {namespace delete test44}

namespace eval test44 {
    proc kk {x} {
    puts $x
    error "Can you see source of THE error?"
  }
}

proc test44::kk2 {} {
  puts "kk2"
}

showVar ::record_definition::db
