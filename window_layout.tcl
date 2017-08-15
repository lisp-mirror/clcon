package require Tk

## Control over window layouts

namespace eval ::window_layout {} 

## windows positions and sizes 
## WINDOW_LAYOUT is a list: { name1 spec1 name2 spec2 ... }
## Name is the name of the layout.
## Spec is a list: { prefix1 geometry_spec1 prefix2 geometry_spec2 ... }
## geometry_spec is a list of { width height left top }
## all values are measured relative to position of maximized "probe" window where its full size is 
## taken to be 1,1 . So there are four quadrants of the screen:
## { 0 0   0.5 0.5 } | { 0.5 0   0.5 0.5 }
## ------------------+--------------------
## { 0 0.5 0.5 0.5 } | { 0.5 0.5 0.5 0.5 }
## values can be > 1 and negative for multi-monitor setups (at least in windows).
## Tools not listed in the layout are opened at some random places. Not all tools currently
## support layouts


proc ::window_layout::SetDefaultWindowLayout {} {
    variable ::tkcon::WINDOW_LAYOUT
    set WINDOW_LAYOUT [list "Default" \
                        [list [::ide_structure::EditorToplevelWindowName] { 0 0 0.5 0.5 } ] \
                        [list . { . 0 0.5 0.5 0.5 } ] \
                        [list [::ide_structure::BufferListBoxWindowName] { 0 0.5 0.5 0.5 } ] \
                        [list [::ide_structure::DebuggerToplevelWindowName] { 0.5 0.5 0.5 0.5 } ] \
                        [list [::ide_structure::ErrorBrowserToplevelWindowName] { 0 0.5 0.5 0.5 } ] \
                        [list .grbrTlv { 0 0.5 0.5 0.5 } ] \
                      ]
}

