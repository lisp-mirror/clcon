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

proc ::win_lay::test1 {} { 
 puts {===============test1============}
 puts {you should see error message like "Failed to start measuring screen"}
 puts {after that you should see "test1 - A succeeded", should not see "test1 - B succeeded"}
 ::win_lay::MeasureScreen [list {} {puts "test1 - A succeeded"}] 
 ::win_lay::MeasureScreen [list {} {puts "test1 - B succeeded"}]
} 

proc ::win_lay::test2 {} { 
 puts {===============test2============}
 puts {you should see error message like "timeout and default screenWidthAvailable ~= 600 "}
 ::win_lay::MeasureScreen [subst -nocommands [list {} {puts "test2 - measuring finished, ScreenWidthAvailable = \$::win_lay::ScreenWidthAvailable"}]]
 variable ::win_lay::MeasuringEndDueTime [ expr [clock milliseconds] - 100000 ]
} 


proc ::win_lay::test3 {} {
 puts {===============test3============}
  puts {you should see 4 windows layed out pretty}
  ::win_lay::MeasureScreen [list {} {::win_lay::Make4Windows}]
}

after 1000 {puts "================= Hold on. Starting tests ... =================="}

after 2000 {::win_lay::test1}

after 4000 {::win_lay::test2}

after 7000 {::win_lay::test3}