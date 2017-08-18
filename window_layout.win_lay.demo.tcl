# demo (manual test) for win_lay package
# idea of test is that windows show w/o gap and w/o big overlap
# run it like this: 
package require Tk

proc ::win_lay::Make4Windows {} {
    variable ScreenWidthAvailable
    variable ScreenHeightAvailable
    variable TotalDecorationWidth
    variable TotalDecorationHeight
    variable XForLeftCornerOfTheScreen
    variable YForTopOfTheScreen

    set ContentWidth [expr $ScreenWidthAvailable/2 - $TotalDecorationWidth ]
    set ContentHeight [expr $ScreenHeightAvailable/2 - $TotalDecorationHeight ]

    MakeSampleWindow .tl 
    wm geometry .tl [::win_lay::ConvertRelativeWindowGeometryToWmGeometryArgumentsOnCurrentScreen {0.4 0.5 0 0 } ]

    MakeSampleWindow .tr
    wm geometry .tr [::win_lay::ConvertRelativeWindowGeometryToWmGeometryArgumentsOnCurrentScreen {0.6 0.5 0.4 0 } ]

    MakeSampleWindow .bl
    wm geometry .bl [::win_lay::ConvertRelativeWindowGeometryToWmGeometryArgumentsOnCurrentScreen {0.4 0.5 0 0.5 } ]

    MakeSampleWindow .br
    wm geometry .br [::win_lay::ConvertRelativeWindowGeometryToWmGeometryArgumentsOnCurrentScreen {0.6 0.5 0.4 0.5 } ]
}

proc ::win_lay::MakeSampleWindow { w } {
  catch {destroy $w}
  toplevel $w ; text $w.t -setgrid 0 ; pack $w.t -padx 5 -pady 5 -fill both
}

::win_lay::MeasureScreen [list {} {::win_lay::Make4Windows}]
