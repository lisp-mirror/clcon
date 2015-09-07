# format for receiving typed messages from lisp and parsing them.

proc ::mprs::TypeTag {x} {
    string index $x 0
}

## ::mprs::Unleash 
# removes type tag of element so that it looks more like tcl data
# for nested structures, you also need to call Unleash for every element.
# See ::mprs::UnleashListOfAtoms
# Note. Nil will not be unleashed to list of zero length. 
# If you not sure your list have elements, use Null afore. 
proc ::mprs::Unleash {x} {
    set tt [TypeTag $x]
    if { $tt eq "\{" } {
        string range $x 2 end-2
        #subst -nocommands -novariables [string range $x 2 end-2]
    } elseif { $tt eq ":" } {
        # problem can be with single keyword as space is added after it.
        if {[string index $x end] eq " "} {
            tr "We get space at the end of keyword $x"
        }
        string range $x 0 end
        #subst -nocommands -novariables [string range $x 0 end]
    } else {
        string range $x 1 end
        #subst -nocommands -novariables [string range $x 1 end]
    }
}


# list is tagged with l or {l . Returns its elements Unleashed. Will work correctly only if all
# elements are atoms
proc ::mprs::UnleashListOfAtoms {typedlist} {
    set TypedElements [Unleash $typedlist]
    lmap x $TypedElements {Unleash $x}
}


## minial testing facility
# tests are runned when code is loading (horrible!)
proc ::mprs::AssertEq {x y {note {}}} {
    if {! ($x eq $y)} {
        ::tkcon::myerror "Assertion failure:$note: $x eq $y"
    }
}

proc ::mprs::SymbolP {x} {
    if {[TypeTag $x] eq "y"} {
        return 1
    } else {
        return 0
    }
}

proc ::mprs::Consp {x} {
    string match {[l\\\{]} [TypeTag $x]
}

# Returns 1, if leashed object is null
proc ::mprs::Null {x} {
    if {$x eq "yCOMMON-LISP:NIL"} {
        return 1
    } else {
        return 0
    }
}

proc ::mprs::Car {x} {
    if {[Consp $x]} {
        lindex [Unleash $x] 0
    } else { ::tkcon::myerror "Car: $x is not a list"
    }
}

proc ::mprs::Cadr {x} {
    if {[Consp $x]} {
        lindex [Unleash $x] 1
    } else { ::tkcon::myerror "Car: $x is not a list"
    }
}

# tests
proc ::mprs::TestFnAutoCons1 {} {
    ::mprs::AssertEq [::mprs::Car [::mprs::Cadr {l:return {l:ok s(format\\ destination\\ control-string\\ &rest\\ format-arguments) } n160 }]] :ok
}

proc ::mprs::TestFnAutoCons2 {} {
    set leashed {l:return {l:ok {lyCOMMON-LISP:NIL {lstrap-errors } } } n118 }
    set EventAsList [Unleash $leashed]
    set oklistL [lindex $EventAsList 1]
    AssertEq [Consp $oklistL] 1 "okListData must be a list"
    set oklist [Unleash $oklistL]
    set okListData [Unleash [lindex $oklist 1]]
    AssertEq [llength $okListData] 2 "okListData must have length 2"
    set localsL [lindex $okListData 0] 
    set trapsL [lindex $okListData 1]
    AssertEq [Consp $localsL] 0 "localsL must not be a list"
    AssertEq [Null $localsL] 1 "localsL must not be null"
    AssertEq [SymbolP $localsL] 1
    AssertEq [Consp $trapsL] 1
    set traps [Unleash $trapsL]
    set trap0 [Unleash [lindex $traps 0]]
    AssertEq $trap0 "trap-errors"
}
