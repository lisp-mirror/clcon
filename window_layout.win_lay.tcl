package require Tk

## Control over window layouts

namespace eval ::win_lay {
  variable MEASURED_SCREEN_GEOMETRY
  variable MEASURED_BORDER_WIDTHS
} 

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
    variable ::tkcon::win_lay
    set win_lay [list "Default" \
                        [list [::ide_structure::EditorToplevelWindowName] { 0.5 0.5 0 0 } ] \
                        [list . { 0.5 0.5 0 0.5 } ] \
                        [list [::ide_structure::BufferListBoxWindowName] { 0.5 0.5 0 0.5 } ] \
                        [list [::ide_structure::DebuggerToplevelWindowName] { 0.5 0.5 0.5 0.5 } ] \
                        [list [::ide_structure::ErrorBrowserToplevelWindowName] { 0.5 0.5 0 0.5 } ] \
                        [list .grbrTlv { 0.5 0.5 0 0.5 } ] \
                      ]
}

# returns a list of width height left top
proc ::win_lay::ParseGeometry {geometry} {
    scan $geometry "%dx%d+%d+%d" width height Left Top
    list $width $height $Left $Top
}    

# MeasureScreen starts a chain of events which fills MEASURED_SCREEN_GEOMETRY
# and_then is a lambda with no arguments smth.like
# [list {} [subst -nocommands {puts \$::win_lay::MEASURED_SCREEN_GEOMETRY}]]
proc ::win_lay::MeasureScreen {and_then} {
    set w .measureScreenBig
    catch {destroy $w}
    toplevel $w
    #catch {wm attributes $w -alpha 0}
    # Trying to position it to the main display, but I have no idea would it work or not
    wm geometry $w 1x1+1+1
    catch {wm state $w zoomed}
    catch {wm attributes $w -zoomed 1}

    update idletasks
    
    after idle [list ::win_lay::MeasureScreenPart2 $and_then]
}

proc ::win_lay::MeasureScreenPart2 {and_then} {
    set w .measureScreenBig
    set geom [wm geometry $w]
    # due to platform issues we don't know if 'top' and 'left' are for content
    # or for decorated window, but we use 'wm geometry' because it is settable. 
    foreach {width height Left Top} [::win_lay::ParseGeometry $geom] break

    set ::win_lay::MEASURED_SCREEN_GEOMETRY [list $width $height $Left $Top]

    set wwLeft [expr $Left+round($width/2)]
    set wwTop [expr $Top+round($height/2)]

    set ww .measureScreenSmall
    catch {destroy $ww}
    toplevel $ww
    #catch {wm attributes $ww -alpha 0}

    wm geometry $ww "10x10+$wwLeft+$wwTop"
    wm deiconify $ww
    raise $ww

    update idletasks

    after idle [list ::win_lay::MeasureScreenPart3 $and_then]
}

proc ::win_lay::MeasureScreenPart3 {and_then} {
    variable ::win_lay::MEASURED_SCREEN_GEOMETRY
    variable ::win_lay::MEASURED_BORDER_WIDTHS
    set w .measureScreenBig
    set ww .measureScreenSmall

    set wgeom [wm geometry $w]
    foreach {wWidth wHeight wLeft wTop} [::win_lay::ParseGeometry $wgeom] break

    ## approximate limits of search
    set wRight [expr $wLeft+$wWidth]
    set wBottom [expr $wTop+$wHeight]

    set wwLeft [winfo rootx $ww]
    set wwTop [winfo rooty $ww]

    # now (try) to find a point which is actually in the window 
    
    # we increment both coords to keep single body of MeasureScreenWalkOverFrame, 
    # but increment on one of coordinates is zero. We start end = start+0.5 for this
    # fixed coordinate to emphasize that we're not expecting to ever achieve loop exit
    # condition
    set lWidth [lindex [::win_lay::MeasureScreenWalkOverFrame \
                        $wwLeft $wwTop -1 0 [expr $wLeft-1] [expr $wTop+0.5]] 0] 
    set tWidth [lindex [::win_lay::MeasureScreenWalkOverFrame \
                        $wwLeft $wwTop 0 -1 [expr $wLeft+0.5] [expr $wTop-1]] 1]
    set rWidth [lindex [::win_lay::MeasureScreenWalkOverFrame \
                        $wwLeft $wwTop 1 0 [expr $wRight+1] [expr $wTop+0.5]] 0]
    set bWidth [lindex [::win_lay::MeasureScreenWalkOverFrame \
                        $wwLeft $wwTop 0 1 [expr $wLeft+0.5] [expr $wBottom+1]] 1]
    
    set sLeft [ expr $wLeft - $lWidth ]
    set sTop [ expr $wTop - $rWidth ]
    set sRight [ expr $wLeft + $wWidth + $rWidth - 1 ]
    set sBottom [ expr $wTop + $wHeight + $bWidth - 1 ]
    set ::win_lay::MEASURED_SCREEN_GEOMETRY [ list $sLeft $sTop $sRight $sBottom ]
    set ::win_lay::MEASURED_BORDER_WIDTHS [list $lWidth $tWidth $rWidth $bWidth]
    puts $::win_lay::MEASURED_SCREEN_GEOMETRY
    puts $::win_lay::MEASURED_BORDER_WIDTHS
    destroy $ww
    destroy $w
    apply $and_then
}

# walks from specified point by increments in X and Y directions until it finds that
# current point is over .measureScreenBig or until it approaches limits
proc ::win_lay::MeasureScreenWalkOverFrame {left top hrzIncrement vrtIncrement hrzLimit vrtLimit} {
    set w .measureScreenBig
    set ww .measureScreenSmall
    set startx ""
    set starty ""
    for { set x $left ; set y $top } { $x != $hrzLimit && $y != $vrtLimit } { incr x $hrzIncrement ; incr y $vrtIncrement } {
      set win ""
      set win [ winfo containing $x $y ]
      # puts $win
      if { [string compare $win $w ] == 0 } {
        set res [list [expr (abs($x-$startx))] [expr (abs($y-$starty))]]
        return $res
      } elseif { [string compare $win $ww ] == 0} {
        # note start point
        set startx $x
        set starty $y
        # do nothing
      } elseif { [string compare $win ""] == 0 } {
        # we're in border, take a breath...
      }
   }
}


# 3 procs are deleted. See window_layout.tcl
# proc ::win_lay::MeasureScreenWalkWhileAboveWindow {ww hrzIncrement vrtIncrement} {
# proc ::win_lay::ContentCoordinates {ww} {
# proc ::win_lay::TranslateRelativeGeometryToCurrentScreen {geometry_spec} {

proc ::win_lay::MakeSampleWindow { w } {
  catch {destroy $w}
  toplevel $w ; text $w.t -setgrid 0 ; pack $w.t -padx 5 -pady 5 -fill both
}

proc ::win_lay::Make4Windows {} {
    variable ::win_lay::MEASURED_SCREEN_GEOMETRY
    variable ::win_lay::MEASURED_BORDER_WIDTHS
    foreach {maximizedX maximizedY maxContentWidth maxContentHeight} $::win_lay::MEASURED_SCREEN_GEOMETRY break
    foreach {wLeft wTop wRight wBottom} $::win_lay::MEASURED_BORDER_WIDTHS break

    # semi-experimental findings, some of them are quite counter-intuitive

    set TitleBarHeight [expr $wTop ]
    showVar TitleBarHeight

    set ScreenWidth [expr $maxContentWidth + $maximizedX - 1]
    set ScreenHeight [expr $maxContentHeight + 2*($maximizedY - 1) + $TitleBarHeight ] 

    showVar ScreenWidth
    showVar ScreenHeight
    # unmaximized window X is measured from $maximizedX
    set DeltaX $maximizedX
    # unmaximized window Y is measured from 0
    set DeltaY 0
    
    set ContentWidth [expr $ScreenWidth/2]
    set ContentHeight [expr $ScreenHeight/2 - $TitleBarHeight ]

    MakeSampleWindow .tl 
    wm geometry .tl "${ContentWidth}x$ContentHeight+[expr $DeltaX + 0*$ScreenWidth/2]+[expr $DeltaY + 0*$ScreenHeight/2]"

    MakeSampleWindow .tr
    wm geometry .tr "${ContentWidth}x$ContentHeight+[expr $DeltaX + 1*$ScreenWidth/2]+[expr $DeltaY + 0*$ScreenHeight/2]"

    MakeSampleWindow .bl
    wm geometry .bl "${ContentWidth}x$ContentHeight+[expr $DeltaX + 0*$ScreenWidth/2]+[expr $DeltaY + 1*$ScreenHeight/2]"

    MakeSampleWindow .br
    wm geometry .br "${ContentWidth}x$ContentHeight+[expr $DeltaX + 1*$ScreenWidth/2]+[expr $DeltaY + 1*$ScreenHeight/2]"

}


