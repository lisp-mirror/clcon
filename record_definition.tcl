
namespace eval ::record_definition {
    # Definition locations database
    variable db [dict create]
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
    # showVar ns
    if {[dict exists $fi file]} {
        # showVar fi
        set QualifiedName [string cat $ns $name]
        dict set db $QualifiedName \
            [dict create file [dict get $fi file] line [dict get $fi line]]
    } else {
        dict unset db $QualifiedName
    }
    set code [list namespace eval $ns [list ::__hidden_real_proc $name $arglist $body ] ] 
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
    if {[info proc ::record_definition::real_proc] ne ""} {
        puts "proc already altered by record_definition"
    } else {
        rename ::proc ::__hidden_real_proc
        rename ::__record_definition_myproc ::proc
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

#catch {namespace delete test44}

#namespace eval test44 {
#  ::record_definition::myproc kk {x} {
#    puts $x
#    error "Can you see source of THE error?"
#  }
#}

