package require Tk

## Control over window layouts

## windows positions and sizes 
## win_lay is a list: { name1 spec1 name2 spec2 ... }
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

proc ::win_lay::SetDefaultWindowLayout {} {
    variable ::tkcon::WINDOW_LAYOUTS
    variable ::tkcon::CURRENT_WINDOW_LAYOUT
    set WINDOW_LAYOUTS [list "Default" \
                        [list [::ide_structure::EditorToplevelWindowName] { 0.5 0.5 0 0 } ] \
                        [list . { 0.5 0.5 0 0.5 } ] \
                        [list [::ide_structure::BufferListBoxWindowName] { 0.5 0.5 0 0.5 } ] \
                        [list [::ide_structure::DebuggerToplevelWindowName] { 0.5 0.5 0.5 0.5 } ] \
                        [list [::ide_structure::ErrorBrowserToplevelWindowName] { 0.5 0.5 0 0.5 } ] \
                        [list .grbrTlv { 0.5 0.5 0 0.5 } ] \
                      ]
    set CURRENT_WINDOW_LAYOUT "Default"
}

# geometry_spec is from win_lay description
proc ::win_lay::ConvertRelativeWindowGeometryToWmGeometryArgumentsOnCurrentScreen {geometry_spec} {
    foreach { w h l t } $geometry_spec break

    variable ScreenWidthAvailable
    variable ScreenHeightAvailable
    variable TotalDecorationWidth
    variable TotalDecorationHeight
    variable XForLeftCornerOfTheScreen
    variable YForTopOfTheScreen

    set IdealW [expr round($w * ($ScreenWidthAvailable)) ]
    set IdealH [expr round($h * ($ScreenHeightAvailable)) ]
    # X and Y of left top corner
    set IdealX [expr round($l * ($ScreenWidthAvailable )) ]
    set IdealY [expr round($t * ($ScreenHeightAvailable )) ]

    set ContentW [expr $IdealW - $TotalDecorationWidth ] 
    set ContentH [expr $IdealH - $TotalDecorationHeight ]
    set Left [expr $IdealX + $XForLeftCornerOfTheScreen ]
    set Top [expr $IdealY + $YForTopOfTheScreen ]

    return "${ContentW}x${ContentH}+$Left+$Top"
}




