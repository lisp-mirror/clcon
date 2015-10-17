# http://wiki.tcl.tk/10702
proc named_args {arguments defaults} {
    upvar 1 "" ""
    array set "" $defaults
    foreach {key value} $arguments {
      if {![info exists ($key)]} {
         error "bad option '$key', should be one of: [lsort [array names {}]]"
      }
      set ($key) $value
    }
}

#proc replace {s args} {
#   named_args $args {-from 0 -to end -with ""}
#   string replace $s $(-from) $(-to) $(-with)
#}
# #--- Testing:
#  % replace suchenwirth -from 4 -to 6 -with xx
#  suchxxirth
#  % replace suchenwirth -from 4 -to 6 -witha xx
#  bad option '-witha', should be one of: -from -to -with

                                                                                
                                                                                
                                                                                
