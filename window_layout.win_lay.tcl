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
## Aread name "~" means that the window is free floating (no position is set by clcon)

proc ::win_lay::SetDefaultWindowLayout {} {
    variable ::tkcon::WINDOW_LAYOUTS
    variable ::tkcon::CURRENT_WINDOW_LAYOUT
    variable WINDOW_LAYOUT_IN_MEMORY
    set WINDOW_LAYOUTS [dict create \
                        "Default" \
                        [dict create \
                          "Geometry-of-areas" \
                          [dict create \
                             "Console-and-location-lists" {0.5 0.5 0 0} \
                             "Error-details-and-help" {0.5 0.3 0 0.5} \
                             "Dialogs" {0.5 0.2 0 0.8} \
                             "Editor-and-inspector" {0.5 0.6 0.5 0} \
                             "Debugger" {0.4 0.8 0 0}] \
                          "Default-area" "Debugger-and-threads" \
                          "Mapping-of-tools-to-areas" \
                          [dict create \
                             [::ide_structure::EditorToplevelWindowName] "Editor-and-inspector" \
                             [::ide_structure::BufferListBoxWindowName] "Editor-and-inspector" \
                             [::ide_structure::DebuggerToplevelWindowName] "Debugger" \
                             [::ide_structure::ErrorBrowserToplevelWindowName] "Console-and-location-lists"  \
                            .grbrTlv "Console-and-location-lists"   \
                            . "Console-and-location-lists"          \
                            .threadList "~"   \
                            .find "Dialogs"  \
                            .completions_menu "Dialogs" \
                            .erbrTv "Error-details-and-help" \
                            .inputString "Dialogs" \
                            .СписокЗакладок "Console-and-location-lists" \
                            .СпрПоСимв "Error-details-and-help" \
                            .ЭкраннаяКлавиатура "Dialogs" \
                            ]]]
    set CURRENT_WINDOW_LAYOUT "Default"
    set WINDOW_LAYOUT_IN_MEMORY [dict get $WINDOW_LAYOUTS $CURRENT_WINDOW_LAYOUT]
}

proc ::win_lay::ExtractToolName {toplevel_name} {
    if {[string compare $toplevel_name .] == 0} {
      return .
    }
    set last_word [lindex [split $toplevel_name "."] end]
    set success [ regexp {^([a-zA-Z0-9]+)_*} $last_word Ignore1 ToolName ]
    if {!$success} {
      error "Unable to extract tool name from $toplevel_name"
    } 
    return [string cat . $ToolName]
}

# Extracts data about this tool from current layout 
# If there is no position recorded for tools of that kind, does nothing
# see also ::powin
proc ::win_lay::PositionATool {toplevel_name} {
    variable WINDOW_LAYOUT_IN_MEMORY
    set wl $WINDOW_LAYOUT_IN_MEMORY
    set ToolName [win_lay::ExtractToolName $toplevel_name]
    puts $ToolName
    if {[dict exists $wl "Mapping-of-tools-to-areas" $ToolName]} {
      set AreaName [dict get $wl "Mapping-of-tools-to-areas" $ToolName]
    } else {
      set AreaName [dict get $wl "Default-area"]
    }
    if {[string compare $AreaName "~"] == 0} {
      return
    }
    set RelativeGeometry [dict get $wl "Geometry-of-areas" $AreaName]
    set geom [::win_lay::ConvertRelativeWindowGeometryToWmGeometryArgumentsOnCurrentScreen $RelativeGeometry]
    wm state $toplevel_name normal
    wm geometry $toplevel_name $geom
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





