# This is an example to tcl/vtk interface. 
# It happens that it can be called via our clcon.
# To achieve that (on Windows) do the following:
# - Build VTK with tcl support
# - Use vtk.exe from your VTK package instead of wish.exe to launch clcon.tcl
# - load this file with ..source c:/<path>/vtk1.tcl
# - to engage Common Lisp to that interface, 
#    define the following function:
# 
# (defun set-cylinder-radius (name r)
#  (assert (> r 0))
#  (clco:eval-in-tcl
#   (format nil "~A SetRadius ~G; $::renWin1 Render" name r)
#   :nowait nil))
#
# - as you have vtk example running, type in in the listener: 
# (set-cylinder-radius '|cylinder2| 2)


# Goal is to show picked actor
    package require vtk
    package require vtkinteraction
    #package require vtktesting

    # catch {load vtktcl}

    # delete old broken pieces
    foreach obj {ren1 цилиндр cylinderMapper cylinderActor 
       cylinder2 cylinderMapper2 cylinderActor2 } {
        catch { $obj Delete }
    }
    catch { destroy .top }

    # Create the GUI: two renderer widgets and a quit button
    # wm withdraw .
    toplevel .top 
    frame .top.f1 

    vtkTkRenderWidget .top.f1.r1 -width 300 -height 300
        ::vtk::bind_tk_render_widget .top.f1.r1

    button .top.btn  -text Close -command [list destroy .top]
    label .top.messageLine

    pack .top.f1.r1 -side left -padx 3 -pady 3 -fill both -expand t
    pack .top.f1  -fill both -expand t
    pack .top.messageLine -fill x    
    pack .top.btn -fill x


    # Get the render window associated with the widget.
    set renWin1 [.top.f1.r1 GetRenderWindow]
    vtkRenderer ren1
    $renWin1 AddRenderer ren1

vtkCylinderSource цилиндр
    цилиндр SetResolution 8

vtkPolyDataMapper cylinderMapper
    cylinderMapper SetInputConnection [цилиндр GetOutputPort]

vtkActor cylinderActor
    cylinderActor SetMapper cylinderMapper
    eval [cylinderActor GetProperty] SetColor {1.0 0.3882 0.2784}
    cylinderActor RotateX  30.0
    cylinderActor RotateY -45.0

vtkCylinderSource cylinder2
    cylinder2 SetResolution 8
    cylinder2 SetCenter 3 3 3

vtkPolyDataMapper cylinderMapper2
    cylinderMapper2 SetInputConnection [cylinder2 GetOutputPort]

vtkActor cylinderActor2
    cylinderActor2 SetMapper cylinderMapper2
    eval [cylinderActor2 GetProperty] SetColor {0.0 1.0 0.0}
    cylinderActor2 RotateX 45.0
    cylinderActor2 RotateY 45.0


    ren1 AddActor cylinderActor
    ren1 AddActor cylinderActor2
    ren1 SetBackground 0.1 0.2 0.4
    $renWin1 SetSize 300 300

# Demonstration of interactive change: 
# cylinder2 SetRadius 1
# $renWin1 Render

    catch { picker Delete }
    vtkPropPicker picker

    proc frame_text_and_scrollbars {frame_pathName} {
        set w $frame_pathName
        frame $w
        text $w.text
        scrollbar $w.sx -orient h -command [list $w.text xview]
        scrollbar $w.sy -orient v -command [list $w.text yview]
        $w.text configure \
            -xscrollcommand [list $w.sx set] \
            -yscrollcommand [list $w.sy set]
        
        # now layout elements in frame
        grid $w.text - $w.sy -sticky news
        grid $w.sx - -sticky ew
        grid columnconfigure $w 0 -weight 1
        grid columnconfigure $w 1 -weight 1
        grid rowconfigure $w 0 -weight 1

        # Don't forget to layout frame itself!
        return $w
    }

    proc PrintVtkObjectToABox {o} {
      set w .boxPrintVtkObjectToABox 
      catch { destroy $w }
      update 
      update idletasks
      toplevel $w
      pack [frame_text_and_scrollbars $w.f]
      $w.f.text insert 1.0 [$o Print]
      wm title $w "$o Print"
      raise $w
    }

    # Renderer will be ren1
    proc doPick {x y Renderer} {
        # Thus we get actual height of renderer in pixels
        # based on http://www.vtk.org/Wiki/VTK/Examples/Cxx/Interaction/Picking
        set r1ActualHeight [lindex [.top.f1.r1 configure -height] 4]
        # We need to reverse y.
        set RightY [expr { $r1ActualHeight - $y }] 
        set picked [picker Pick $x $RightY 0 $Renderer]
        if {$picked} { 
          set pickedActor [picker GetActor]
          foreach {xx yy zz} [picker GetPickPosition] {}
          set message [format "You picked at %.3g %.3g %.3g - %s : %s" \
                         $xx $yy $zz $pickedActor [$pickedActor GetClassName]]
          PrintVtkObjectToABox $pickedActor
        } else {
          set message {}
        }
        .top.messageLine configure -text $message
    }

    bind .top.f1.r1 <Button-3> "doPick %x %y ren1; break"

# Поменять параметр красного цилиндра:
# цилиндр SetRadius 1
# $renWin1 Render

