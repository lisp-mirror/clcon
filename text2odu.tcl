# sending text commands to oduvanchik
# See also text2odu-from-tcl-to-queue.lisp , clcon_text.tcl

namespace eval ::text2odu {

    # Convert text index to oduvanchik coordinate system
    # And quotes for lisp
    proc CoerceIndex {txt index} {
        # FIXME we might want pass 'begin' and 'end' as is and process
        # them specially on lisp side. 
        switch -exact $index {
            {} { set i2 {} }
            begin - 
            end { set i2 [$txt index $index] }
            insert { set i2 [$txt index $index] }
            default {
                if {[regexp {^[0-9]+\.[0-9]+$} $index]} {
                    set i2 $index
                } else {
                    set i2 [$txt index $index]
                }
            }
        }
        set qIndex [::tkcon::QuoteLispObjToString $i2]
        return $qIndex
    }
}
