# Synonyms for roInsert, roDelete which can be used with text and clcon_text

namespace eval ro_out {}

proc ::ro_out::I {self args} {
    set c [winfo class $self]
    switch -exact $c {
        Btext {
            eval "$self RoInsert $args"
        }
        Text {
            eval "$self insert $args"
        }
        default {
            error "unknown class $c for ro_out"
        }
    }
}

proc ::ro_out::D {self args} {
    set c [winfo class $self]
    switch -exact $c {
        Btext {
            eval "$self RoDelete $args"
        }
        Text {
            eval "$self delete $args"
        }
        default {
            error "unknown class $c for ro_out"
        }
    }
}
