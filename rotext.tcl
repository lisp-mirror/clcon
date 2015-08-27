## Utility
# May be messed up by namespace issues and
# May go wrong if $p contains pattern special characters
proc ProcExistsP p {
    set SimilarProcs [info procs $p]
    puts "SimilarProcs ($p) = $SimilarProcs"
    return uplevel 1 [expr {[lsearch -exact $SimilarProcs $p] >= 0}]
}


## SetTextReadonly
# This function must be called one time for any text
# widget we create in clcon
# If ReadonlyP ==1, this makes text widget readonly,
# keeping an ability to receive focus
# and a visible cursor for navigation.
# After making text readonly,
# text can still be manipulated with
# RoInsert, RoDelete, RoReplace
# If ReadonlyP == 0, this makes text widget readwrite.
# RoInsert, RoDelete, RoReplace can still be used
# Result: returns pathname
# Side effect: makes text readonly.
# Creates widget $pathname.ro-In
proc InitTextReadonly { pathName ReadonlyP } {

    rename $pathName $pathName.ro-INtErNaL

    if {$ReadonlyP} {
        set widget_proc_body_pattern {
            switch -exact -- [lindex $args 0] {
                insert {}
                delete {}
                replace {}
                RoInsert {
                    return [eval <pathName>.ro-INtErNaL insert [lrange $args 1 end]]
                }
                RoDelete {
                    return [eval <pathName>.ro-INtErNaL delete [lrange $args 1 end]]
                }
                RoReplace {
                    return [eval <pathName>.ro-INtErNaL replace [lrange $args 1 end]]
                }
                ReadonlyP {
                    return 1
                }
                default { 
                    return [eval <pathName>.ro-INtErNaL $args] 
                }
            }
        }
    } else {
        # for not ReadonlyP widgets, both insert and RoInsert will work
        set widget_proc_body_pattern {
            switch -exact -- [lindex $args 0] {
                RoInsert {
                    return [eval <pathName>.ro-INtErNaL insert [lrange $args 1 end]]
                }
                RoDelete {
                    return [eval <pathName>.ro-INtErNaL delete [lrange $args 1 end]]
                }
                RoReplace {
                    return [eval <pathName>.ro-INtErNaL replace [lrange $args 1 end]]
                }
                ReadonlyP {
                    return 0
                }
                default { 
                    return [eval <pathName>.ro-INtErNaL $args] 
                }
            }
        }
    }
    set widget_proc_body [regsub -all <pathName> $widget_proc_body_pattern $pathName]
    proc $pathName {args} $widget_proc_body

    return $pathName
}

######################## Example ################################
# text .text
# InitTextReadonly .text 1
    
# .text insert 0.0 "this line will be ignored"

# for {set x 0} {$x<10} {incr x} { 
#     .text RoInsert end "line $x\n"
# }

# text .text2
# InitTextReadonly .text2 0
# .text2 insert end "Line inserted by 'insert'\n"
# .text2 RoInsert end "Line inserted by 'RoInsert'\n"

# pack .text   -side top
# pack .text2 -side bottom
