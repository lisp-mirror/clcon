## SLIME's find definition for tcl
#
# Keeping track of places where 'proc' is invoked
# With ::record_definition::EditProcedure we can jump to definition
# of any procedure loaded after this file, e.g. snit, tablelist and
# all our application code.
#
# INSTALLATION: just load this file to your tcl world
# as early as possible (e.g. before requiring any packages)
#
# This file is part of https://bitbucket.org/budden/clcon project
# but it should be easy to seprate it provided you write your own
# editor function.
# 
#
## MIT License
## (C) Denis Budyak 2015


# Args: name of variable without $, verbatim. E.g. showVar myvar
# Side effects: prints <variable-name> = <variable value>
# See also showVarPutd defined in util.tcl
proc showVar {name} {
      upvar 1 $name local
      puts "sV:$name=$local" 
  }


## A la common lisp's defvar, http://www.lispworks.com/documentation/lw60/CLHS/Body/m_defpar.htm
# Assigns value to a variable only if variable does not "info exists"
proc defvar {name delicate_value} {
    if {![uplevel 1 [list info exists $name]]} {
        uplevel 1 [list set $name $delicate_value]
    } else {
        return -code error "variable '$name' already exists"
    }
}

# proc defvar {name delicate_value} {
#     if {$delicate_value eq ""} {
#         set rhs_code "{}"
#     } else {
#         set rhs_code $delicate_value
#     }
#     set code [subst -nocommands {
#         uplevel 1 {
#             variable $name
#             if {![info exists $name]} {
#                 set $name $rhs_code
#             }
#         }
#     }]
#     eval $code
# }




namespace eval ::record_definition {
    # Definition locations database
    defvar db [dict create]

    # I failed to find renamed proc with [info proc]. So
    # I introduce global flag
    # 1 means proc was altered (see ::record_definition::AlterProc)
    defvar proc_was_renamed 0
}

proc ::__record_definition_myproc {name arglist body} {
    variable ::record_definition::db
    set level [expr { [ info frame ] - 1 }]
    set fi [ info frame $level ] 
    set ns [uplevel { namespace current }]
    if {$ns ne "::"} {
        set QualifiedName [string cat $ns "::" $name]
    } elseif { [string range $name 0 1] ne "::" } {
        set QualifiedName [string cat "::" $name]
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
    eval $code
    return {}
}

# Modifies proc no more than once
proc ::record_definition::AlterProc {} {
    variable proc_was_renamed
    # showVar proc_was_renamed
    if {$proc_was_renamed} {
        puts "proc was already altered by record_definition::AlterProc"
    } else {
        rename ::proc ::__hidden_real_proc
        rename ::__record_definition_myproc ::proc
        set proc_was_renamed 1
    }
}

::record_definition::AlterProc

# This example works in context of https://bitbucket.org/budden/clcon project only.
# For your project, use your own editor function
proc ::record_definition::EditProcedure {QualifiedName} {
    variable db
    set location_info_exists [dict exists $db $QualifiedName]
    if {$location_info_exists} {
        set li [dict get $db $QualifiedName]
        ::tkcon::EditFileAtLine [dict get $li file] [dict get $li line]
    } else {
        ::edt::edit -type proc -- $QualifiedName
    }
}


## Tests: 
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
# catch {namespace delete test44}
# namespace eval test44 {
#     proc kk {x} {
#     puts $x
#     error "Can you see source of THE error?"
#   }
# }
# proc test44::kk2 {} {
#   puts "kk2"
# }
# showVar ::record_definition::db
## End of tests
