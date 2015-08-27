## make_text_readonly
# Permanently makes text widget readonly,
# keeping an ability to receive focus
# and a visible cursor for navigation.
# After making text readonly,
# text can still be manipulated with
# really_insert, really_delete, really_replace
proc make_text_readonly { pathName } {

    rename $pathName $pathName.internal

    set body {
        switch -exact -- [lindex $args 0] {
            insert {}
            delete {}
            replace {}
            really_insert {
                return [eval <pathName>.internal insert [lrange $args 1 end]]
            }
            really_delete {
                return [eval <pathName>.internal delete [lrange $args 1 end]]
            }
            really_replace {
                return [eval <pathName>.internal replace [lrange $args 1 end]]
            }
            default { 
                return [eval <pathName>.internal $args] 
            }
        }
    }

    set body2 [regsub -all <pathName> $body $pathName]
    
    proc $pathName {args} $body2
}

######################## Example ################################
#text .text
#
#make_text_readonly .text
#
#.text insert 0.0 "this line will be ignored"
# for {set x 0} {$x<10} {incr x} { 
#     .text really_insert end "line $x\n"
# }

# text .text2

# pack .text   -side top
# pack .text2 -side bottom
