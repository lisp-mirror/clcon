# (C) Denis Budyak 2017, include MIT License here
# Measuring of the screen size, so that we could apply a window layout to that screen

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
    ## variables below are explained in ::win_lay::MeasureScreenPart3
    variable ScreenWidthAvailable
    variable ScreenHeightAvailable
    variable TotalDecorationWidth
    variable TotalDecorationHeight
    variable XForLeftCornerOfTheScreen
    variable YForTopOfTheScreen
} 



## SetDefaultScreenSizes
## we set them before any measurement, and we fall back to 
## them if measuring event chain failed. 
## we assume 640х480, and toolbars are of width 40
proc ::win_lay::SetDefaultScreenSizes {} {
    variable ScreenWidthAvailable 600 
    variable ScreenHeightAvailable 440
    variable TotalDecorationWidth 0 
    variable TotalDecorationHeight 40
    variable XForLeftCornerOfTheScreen 0
    variable YForTopOfTheScreen 0
}

# returns a list of width height left top
proc ::win_lay::ParseGeometry {geometry} {
    scan $geometry "%dx%d+%d+%d" width height Left Top
    list $width $height $Left $Top
}    
# MeasureScreen tries to measure a screen and then run `and_then` lambda 
# If measuring process is going on already, prints a warning and returns 0.
# Otherwise starts new chain of events to measure screen and run `and_then`, 
# sets up a timeout  and returns 1. If measuring times out, 
# default screen sizes are set and and_then is executed
# After measuring, `and_then` will be executed. If measuring succeeded, `and_then`
# will see actual screen sizes. If measuring would time out (after (MeasuringTimeoutMs)), 
# some default sizes will be seen
# `and_then` is a lambda with no arguments, smth. like
# [list {} [subst -nocommands {puts \$::win_lay::TitleBarHeight}]]
#
# Couple invisible windows are created. 
# If user would click somewhere and modify the order
# of windows, many kinds of bad things would occur. This is why we use a timeout guard.

proc ::win_lay::MeasureScreen {and_then} {

    if {![::win_lay::StartMeasuring $and_then]} {
      puts stderr "Failed to start measuring screen. Wait couple seconds until previous attempt to measure screen ends and retry"
      return 0
    }

    set w .measureScreenBig
    catch {destroy $w}
    toplevel $w
    #catch {wm attributes $w -alpha 0}
    # Trying to position it to the main display, but I have no idea would it work or not
    wm geometry $w 1x1+1+1
    catch {wm state $w zoomed}
    catch {wm attributes $w -zoomed 1}

    update idletasks
    
    after idle {::win_lay::MeasureScreenPart2}
    return 1
}

proc ::win_lay::MeasureScreenPart2 {} {

    if {[::win_lay::CheckIfMeasuringFinished]} {
      return
    }

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

    after idle {::win_lay::MeasureScreenPart3}
}

proc ::win_lay::MeasureScreenPart3 {} {

    # X and Y position for `wm geometry` to position window at the left-top corner of the screen
    variable XForLeftCornerOfTheScreen
    variable YForTopOfTheScreen

    if {[::win_lay::CheckIfMeasuringFinished]} {
      return
    }

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
    
    #puts [list $maximizedX $maximizedY $maxContentWidth $maxContentHeight]


    # semi-experimental findings, some of them are quite counter-intuitive

    variable TitleBarHeight [expr $brdrWdthT ]

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

    ::win_lay::FinishMeasuring
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
        # we're in border, wait...
      }
   }
}

::win_lay::SetDefaultScreenSizes

