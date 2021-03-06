# We want to look for a substring in a string
# tcl only allows glob and regexp search.
# So lets quote all regexp special characters
proc QuoteStringForRegexp {s} {
    regsub -all {[][{}()\\^$*+?.]} $s {\\&} s
    return $s
}


proc UnixFileNameToWindows {fn} {
    regsub -all {(/)} $fn {\\} fn
    return $fn
}

proc WindowsFileNameToUnix {fn} {
    regsub -all {(\\)} $fn {/} fn
    return $fn
}
    
proc CountOccurancesOfSubstring {substring string} {
    set substring [QuoteStringForRegexp $substring]
    return [llength [regexp -all -inline (?=$substring) $string]]
}

proc ProcedureNop {args} {
}

## Utility
# May be messed up by namespace issues and
# May go wrong if $p contains pattern special characters
proc ProcExistsP p {
    error "I'm untested yet"
    set SimilarProcs [info procs $p]
    puts "SimilarProcs ($p) = $SimilarProcs"
    return uplevel 1 [expr {[lsearch -exact $SimilarProcs $p] >= 0}]
}


# See also defvar defined in record_definition.tcl

# See also showVar defined in record_definition.tcl
proc showVarPutd {name} {
    upvar 1 $name local
    putd "sV:$name=$local" 
}


proc SubstituteSingleValueInListVarKeyEq {listVariable old new} {
    upvar 1 $listVariable var
    set idx [lsearch -exact $var $old]
    if {$idx >= 0} {
        set var [lreplace $var $idx $idx [list $new]]
    }
}

# Allows for named counters.
proc GenNamedCounter {name} {
    set VarName ContinuationCounter$name
    upvar \#0 $VarName Counter
    global $VarName
    if {![info exists $VarName]} {
        set Counter 1
    } else {
        set Counter [expr {$Counter + 1}]
    }
    return $Counter
}



# tests
# puts [CountOccurancesOfSubstring "a" "babab"]
# 2
# puts [CountOccurancesOfSubstring "a*" "ba*bab"]
# 1
# puts [CountOccurancesOfSubstring "a*" "ba*ba*b"]
# 2



proc tr {x} {
    catch { throw {ARITH DIVZERO {divide by zero}}} 
    tk_messageBox -message "$x [info errorstack]"
}


proc dump_all_stack_putd {{prefix "  stack:"}} {
    set i 0
    while { $i < [ info frame ] } {
        set inf [info frame $i]
        putd "$prefix$inf"
        incr i
    }
}

proc BooleanToAsterik {b} {
    if {$b} {
        return "*"
    } else {
        return ""
    }
}        


proc ExtractValuesFromNumberedInitializator {list} {
    set i 0 
    set result [list]
    foreach l $list {
        if { $i % 2 == 1 } {
           lappend result $l
        } else {
           ::mprs::AssertEq $l [expr {$i / 2}]
        }
        incr i
    }
    return $result
}
    
