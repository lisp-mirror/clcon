package require Tk

## Control over window layouts

# Background: 
# In Windows 10, tk creates a transparent frame of width 10 around the window.
# When a window is active, it also has a visible frame of width 1 (in my theme), 
# so 9 is left transparent
# When a window is maximized, both invisible and visible frames are moved beyond
# the screen bounds. When windows are tiled, invisible frames of neighbouring windows overlap,
# but visible ones are put side by side. On other platforms, e.g. Windows Server 2003, 
# window has a non-transparent frame. I found no way to obtain the actual width of visible frame. 
# So it is impossible to find out actual window size programmatically w/o calling WinAPI.
# So I do what I can and let visible frames overlap (this is good in fact because in Win10 
# frames are transparent and because active window get a bit greater visually than inactive
# windows.
# See also:
# "tk centering window" http://wiki.tcl.tk/1254
# "tk total window geometry" http://wiki.tcl.tk/11291 
# https://www.linux.org.ru/forum/development/13617877
# https://stackoverflow.com/questions/33231484/python-tkinter-how-do-i-get-the-window-size-including-borders-on-windows


namespace eval ::win_lay {
  # wm geometry from zoomed window
  variable ZOOMED_WINDOW_GEOMETRY
  # we meausure the shadow of a small normal window's border on zoomed window
  # measured frame 
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

# MeasureScreen starts a chain of events which fills ZOOMED_WINDOW_GEOMETRY
# and_then is a lambda with no arguments smth.like
# [list {} [subst -nocommands {puts \$::win_lay::ZOOMED_WINDOW_GEOMETRY}]]
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

    # set ::win_lay::ZOOMED_WINDOW_GEOMETRY [list $width $height $Left $Top]

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
    variable ZOOMED_WINDOW_GEOMETRY
    variable MEASURED_BORDER_WIDTHS

    # X and Y position for `wm geometry` to position window at the left-top corner of the screen
    variable XForLeftCornerOfTheScreen
    variable YForTopOfTheScreen

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
    # condition through this direction
    set brdrWdthL [lindex [::win_lay::MeasureScreenWalkOverFrame \
                        $wwLeft $wwTop -1 0 [expr $wLeft-1] [expr $wTop+0.5]] 0] 
    set brdrWdthT [lindex [::win_lay::MeasureScreenWalkOverFrame \
                        $wwLeft $wwTop 0 -1 [expr $wLeft+0.5] [expr $wTop-1]] 1]
    set brdrWdthR [lindex [::win_lay::MeasureScreenWalkOverFrame \
                        $wwLeft $wwTop 1 0 [expr $wRight+1] [expr $wTop+0.5]] 0]
    set brdrWdthB [lindex [::win_lay::MeasureScreenWalkOverFrame \
                        $wwLeft $wwTop 0 1 [expr $wLeft+0.5] [expr $wBottom+1]] 1]
    
    set maximizedX [ expr $wLeft - $brdrWdthL ]
    set maximizedY [ expr $wTop - $brdrWdthR ]
    set maxContentWidth [ expr $wLeft + $wWidth + $brdrWdthR - 1 ]
    set maxContentHeight [ expr $wTop + $wHeight + $brdrWdthB - 1 ]
    
    puts [list $maximizedX $maximizedY $maxContentWidth $maxContentHeight]


    # semi-experimental findings, some of them are quite counter-intuitive

    variable TitleBarHeight [expr $brdrWdthT ]
    showVar TitleBarHeight

    #
    # available width and height of the screen (obtained from measuring a zoomed window)
    # note that in some multi-monitor setups `winfo ScreenHeight` and `winfo ScreenWidth` give
    # a total size of a desktop combined from all windows, which is not suitable for tiling
    # windows because a part of this cumulative desktop is actually invisible
    # also winfo does not give information about task bar space. So maximizing window and measuring
    # it is a way to obtain values for at least one (generally speaking, random) display
    #
    variable ScreenWidthAvailable [expr $maxContentWidth + $maximizedX - 1]
    variable ScreenHeightAvailable [expr $maxContentHeight + 2*($maximizedY - 1) + $TitleBarHeight ] 

    variable XForLeftCornerOfTheScreen $maximizedX

    # unmaximized window Y is measured from 0, but we add empirical
    # offset because frame is transparent in Win 10
    set EmpricalOffsetDueToTransparencyOfFrame 1

    variable YForTopOfTheScreen [expr -$EmpricalOffsetDueToTransparencyOfFrame ]

    # total width and height of decoration of the frame
    # we can not calculate it precisely, so we use some empirical apprach, and we allow
    # windows to overlap. We can't control how far do they overlap
    variable TotalDecorationWidth 0 
    variable TotalDecorationHeight [expr $TitleBarHeight - 2*$EmpricalOffsetDueToTransparencyOfFrame]

    destroy $ww
    destroy $w
    apply $and_then
}

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


# geometry_spec is from win_lay description
proc ::win_lay::ConvertRelativeWindowGeometryToWmGeometryArgumentsOnCurrentScreen {geometry_spec} {
    foreach { w h l t } $geometry_spec break

    variable ScreenWidthAvailable
    variable ScreenHeightAvailable
    variable TotalDecorationWidth
    variable TotalDecorationHeight
    variable XForLeftCornerOfTheScreen
    variable YForTopOfTheScreen

    # +1 and -1 here is to make ideal windows be non-overlapped and make them cover the
    # whole screen. So we yield bottom and right row of pixels to the next window on 
    # bottom or on the right of this window, and add a virtual bottom and right row to the
    # available screen area. 
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


# some procs are deleted. See window_layout.tcl
# proc ::win_lay::MeasureScreenWalkWhileAboveWindow {ww hrzIncrement vrtIncrement} {
# proc ::win_lay::ContentCoordinates {ww} {

proc ::win_lay::MakeSampleWindow { w } {
  catch {destroy $w}
  toplevel $w ; text $w.t -setgrid 0 ; pack $w.t -padx 5 -pady 5 -fill both
}
