# ide structure - toplevel windows. 

namespace eval ::ide_structure {
}

proc ::ide_structure::DebuggerToplevelWindowName {} {
    variable ::tkcon::PRIV
    return $PRIV(base).ldbgTlv
}
    
proc ::ide_structure::EditorToplevelWindowName {} {   
    variable ::tkcon::PRIV
    string cat $PRIV(base) .editorry
}
        

proc ::ide_structure::BufferListBoxWindowName {} {
    variable ::tkcon::PRIV
    return $PRIV(base).buliTlv
}
