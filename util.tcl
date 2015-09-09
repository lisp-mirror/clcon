# We want to look for a substring in a string
# tcl only allows glob and regexp search.
# So lets quote all regexp special characters
proc QuoteStringForRegexp {s} {
    regsub -all {[][{}()\\^$*+?.]} $s {\\&} s
    return $s
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


# tests
# puts [CountOccurancesOfSubstring "a" "babab"]
# 2
# puts [CountOccurancesOfSubstring "a*" "ba*bab"]
# 1
# puts [CountOccurancesOfSubstring "a*" "ba*ba*b"]
# 2
