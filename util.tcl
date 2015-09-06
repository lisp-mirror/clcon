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

# tests
# puts [CountOccurancesOfSubstring "a" "babab"]
# 2
# puts [CountOccurancesOfSubstring "a*" "ba*bab"]
# 1
# puts [CountOccurancesOfSubstring "a*" "ba*ba*b"]
# 2
