# format for receiving typed messages from lisp and parsing them.

proc ::mprs::TypeTag {x} {
    string index $x 0
}

## ::mprs::Unleash 
# removes type tag of element so that it looks more like tcl data
# for nested structures, you also need to call Unleash for every element.
# See ::mprs::UnleashListOfAtoms
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
proc ::mprs::AssertEq {x y} {
    if {! ($x eq $y)} {
        ::tkcon::myerror "Assertion failure: $x eq $y"
    }
}

proc ::mprs::Consp {x} {
    string match {[l\\\{]} [TypeTag $x]
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

# test
::mprs::AssertEq [::mprs::Car [::mprs::Cadr {l:return {l:ok s(format\\ destination\\ control-string\\ &rest\\ format-arguments) } n160 }]] :ok
