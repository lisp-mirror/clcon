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
}

# For unnecessary complexity, we don't update layout database directly, but use a buffer
# named WINDOW_LAYOUT_IN_MEMORY. So we need load/save operations for it. 
proc ::win_lay::LoadCurrentLayoutFromDatabase {} {
    variable ::tkcon::WINDOW_LAYOUTS
    variable ::tkcon::CURRENT_WINDOW_LAYOUT
    variable WINDOW_LAYOUT_IN_MEMORY [dict get $WINDOW_LAYOUTS $CURRENT_WINDOW_LAYOUT]
}

proc ::win_lay::SaveCurrentLayoutToDatabase {} {
    variable ::tkcon::WINDOW_LAYOUTS
    variable ::tkcon::CURRENT_WINDOW_LAYOUT
    variable WINDOW_LAYOUT_IN_MEMORY 
    dict set WINDOW_LAYOUTS $CURRENT_WINDOW_LAYOUT $WINDOW_LAYOUT_IN_MEMORY 
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
    #puts $ToolName
    if {[dict exists $wl "Mapping-of-tools-to-areas" $ToolName]} {
      set AreaName [dict get $wl "Mapping-of-tools-to-areas" $ToolName]
    } else {
      set AreaName [dict get $wl "Default-area"]
    }
    #puts $AreaName
    if {[string compare $AreaName "~"] == 0} {
      return
    }
    set RelativeGeometry [dict get $wl "Geometry-of-areas" $AreaName]
    ::win_lay::PositionAWindowAtRelativeGeometry $toplevel_name $RelativeGeometry
}

proc ::win_lay::PositionAWindowAtRelativeGeometry {toplevel_name RelativeGeometry} {
    set geom [::win_lay::ConvertRelativeWindowGeometryToWmGeometryArgumentsOnCurrentScreen $RelativeGeometry]
    wm state $toplevel_name normal
    wm geometry $toplevel_name $geom
}

proc ::win_lay::RecordLayout {toplevel_name} {
    variable WINDOW_LAYOUT_IN_MEMORY
    variable ::tkcon::PRIV
    set wl $WINDOW_LAYOUT_IN_MEMORY
    set w $toplevel_name
    set title [wm title $w]
    set ToolName [::win_lay::ExtractToolName $toplevel_name]

    set RelativeGeometry [::win_lay::ConvertWindowGeometryToRelativeWindowGeometry $w]

    puts "Welcome to location saving wizard. "
    puts "You could also edit you $PRIV(desktopfile) by thirdparty editor when clcon is down"
    puts "Сlcon layout manager classifies windows by their tool name (e.g editor, console, debugger, or find dialog have different tool names)"
    puts "Clcon auto-places windows to one of several places on the screeen, called 'areas'. Areas are similar to dock containers in docking window managers. One or more tool names can be mapped to one area. Windows with that tool names are auto-placed at that 'area's screen coordinates upon creation."
    puts "Window entitled «$title» has a tool name '$ToolName'"
    if {![dict exists $wl "Mapping-of-tools-to-areas" $ToolName]} {
        puts stderr $wl
        puts "Location saving for tool name '$ToolName' is not implemented (your patch is welcome)"
        return
    }
    set AreaName [dict get $wl "Mapping-of-tools-to-areas" $ToolName]
    puts "Tool named '$ToolName' is currently mapped to area named '$AreaName'"
    if {$AreaName eq $ToolName} {
        puts "Do you want to save current location of «$title» as a location for new windows for that area (type 1=Yes,9=No,<Return>=Cancel)?"
        # (gets stdin) would be prettier, but I dont know how to remove a prompt
        set answer [lindex [::LameAskForLispExpression .tab1 "1 or 9"] 1]
        if {$answer eq 1} {
            dict set $wl "Geometry-of-areas" $AreaName $RelativeGeometry
        } else {
            puts "window location is not saved"
        } 
    } elseif {$AreaName eq "~"} {
        puts "Tool name '$ToolName' is currently bound to area named '~'. That means the window is free-floating and positions itself whenever it wants (at random places)"
        puts "Enter your choice: 5: Create a new area named '$ToolName' for this tool and save its location"
        puts "                   9: Don't save location"
        puts "                   <RETURN>: Cancel"
        set answer [lindex [::LameAskForLispExpression .tab1 "5 or 9"] 1]
        if {$answer eq 5} {
            dict set WINDOW_LAYOUT_IN_MEMORY "Geometry-of-areas" $ToolName $RelativeGeometry
        } else {
            puts "window location is not saved" 
        }
    } else {
        puts "Enter your choice: 1: Save location of «$title» as a location for area '$AreaName'"
        puts "                   5: Create a new area named '$ToolName' for this tool and save its location"
        puts "                   9: Don't save location"
        puts "                   <RETURN>: Cancel"
        set answer [lindex [::LameAskForLispExpression .tab1 "1,5 or 9"] 1]
        if {$answer eq 1} {
            dict set WINDOW_LAYOUT_IN_MEMORY "Geometry-of-areas" $AreaName $RelativeGeometry
        } elseif {$answer eq 5} {
            dict set WINDOW_LAYOUT_IN_MEMORY "Geometry-of-areas" $ToolName $RelativeGeometry
        } else {
            puts "window location is not saved" 
        }
    }
}
    


