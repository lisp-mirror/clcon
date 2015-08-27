## This is just pure tcl/tk application
## 

package require tablelist

# FIXME remove it when embedded into main app
lappend ::auto_path [file join [file dirname [info script]] "lib/wcb3.5"]
package require wcb

proc WidgetParent { w } {
    set lst [split $w .]
    set sublist [lrange $lst 0 end-1]
    puts "sublist = $sublist"
    return [join $sublist .]
}
    
catch {font create tkconfixed -family Courier -size -20}
#	    $con configure -font tkconfixed

# Replace this at integration stage
proc ConfigureTextFonts {text} {
    $text configure \
        -foreground \#000000 \
        -insertbackground \#000000 \
        -font tkconfixed -borderwidth 1 -highlightthickness 0 \
        -undo 1
}

#        -background {} \


proc InsertDataToShowOrBeep { w EventAsList } {
    # bind var for convenience

    set b [BodyTextOfErrorBrowser $w]

    # clear old data if it existed

    [TitleOfErrorBrowser $w] delete 0.0 end
    $b delete 0.0 end
   
    # and now insert what we have parsed
    
    [TitleOfErrorBrowser $w] insert 0.0 "WOW1"
    $b insert 0.0 "I don't know what is that"
}




proc InsertNewText {idx} {
    global data
    global tv
           
    set item [lindex $data $idx]
    set MyCode [dict get $item {details-code}]

    EnsureTextView
    $tv.body.text delete 0.0 end
    $tv.body.text insert end $MyCode
    event generate $tv <<GoToTop>>
}


proc DoOnSelect {tbl idx} {
    after idle [InsertNewText $idx]
}


proc InitData { EventAsList } {
    global data
    
    set data [list {title "error 1" details-code {
                  $w RoInsert 0.0 "wow";
                  $w RoInsert end "ura"
              }} {title "error 2" details-code {
                  $w RoInsert 0.0 "2222";
              }}
             ]

}
        
# This is a contiuation assigned on reply on initialization request 
proc SwankBrowseErrors1 { EventAsList } {
    global tv
    global MyActiveIndex

    InitData $EventAsList
    
    set w [PrepareGui1]

    set MyActiveIndex -1

    EnsureTextView

    set tbl $w.tf.tbl
    
    wcb::callback $tbl before activate DoOnSelect
    
#    bind $w.tf.tbl <<TablelistSelect>> [list DoOnSelect $w.tf.tbl]
#    bind $w.tf.tbl <<TablelistCellUpdated>> [list DoOnSelect $w.tf.tbl]
#    bind $w.tf.tbl <<ListBoxSelect>> [list DoOnSelect $w.tf.tbl]

    focus $tbl

    FillHeaders $tbl
    
    DoOnSelect $tbl 0
    # InsertDataToShowOrBeep $w $EventAsList
}

proc DoGoToTop {w} {
    wm deiconify $w
    raise $w
    #tk_messageBox -message "WOW"
}

proc EnsureTextView {} {
    global tv
    set tv .tv
    set w $tv

    if {[winfo exists $tv]} {
        return
    }
    
    toplevel $w
    wm title $w "Error details"
    frame $w.body
    text $w.body.text

    ConfigureTextFonts $w.body.text
    $w.body.text configure -yscrollcommand [list $w.body.sy set] 
    catch {
	# 8.5+ stuff
	set tabsp [expr {$OPT(tabspace) * [font measure $OPT(font) 0]}]
	$w.body.text configure -tabs [list $tabsp left] -tabstyle wordprocessor
    }

    #scrollbar $w.body.sx -orient h -command [list $w.body.text xview]
    scrollbar $w.body.sy -orient v -command [list $w.body.text yview]
    
    grid $w.body.text - $w.body.sy -sticky news
    #grid $w.body.sx - -sticky ew
    grid columnconfigure $w.body 0 -weight 1 
    grid columnconfigure $w.body 1 -weight 1
    grid rowconfigure $w.body 0 -weight 1 

    pack $w.body -fill both -expand 1

    bind $w <<GoToTop>> [list DoGoToTop $w]

    return

}
    
# Fills browser from data 
proc FillHeaders {tbl} {
    global data
    foreach item $data {
        set title [dict get $item {title}]
        $tbl insert end [list $title]
    }
}



# Make toplevel widget and its children 
proc PrepareGui1 {} {
    # Create unique edit window toplevel
    if { 1 == 0 } {
        set w .__inspector
        set i 0
        while {[winfo exists $w[incr i]]} {}
        append w $i
        toplevel $w
        wm withdraw $w
        
        # title 
        set word "Error browser"
        if {[string length $word] > 20} {
            wm title $w "[string range $word 0 16]... - tkcon Edit"
        } else {
            wm title $w "$word - tkcon Edit"
        }
    } else {
        set w ""
    }

    # Widget root
    set root $w 

# --------------------------------- frames-----------------              

# paned window

    frame $w.tf
    set tbl $w.tf.tbl
    
    tablelist::tablelist $tbl -columns {20 ""} -stretch all -spacing 10

    $tbl configure \
        -foreground \#000000 \
        -font tkconfixed -borderwidth 1 -highlightthickness 0
        

    $tbl columnconfigure 0 -wrap true  
    
    set f1 $w.tf
    scrollbar $f1.sy -orient v -command [list $tbl yview]
    $tbl configure -yscrollcommand [list $f1.sy set]
    grid $tbl - $f1.sy -sticky news
    grid columnconfigure $f1 0 -weight 1
    grid columnconfigure $f1 1 -weight 1
    grid rowconfigure $f1 0 -weight 1

    pack $f1 -side top -fill x

    # other frame and text

#    pack $w.pane -side top -expand yes -fill both -pady 2 -padx 2m

    return $w    
}

proc TitleOfErrorBrowser {w} {
    set f1 $w.pane.title
    set f2 $w.pane.body
    return $w.pane.title.text
}

proc BodyTextOfErrorBrowser {w} {
    set f1 $w.pane.title
    set f2 $w.pane.body
    return $f2.text
}

proc DebugStartup {} {
    SwankBrowseErrors1 {'defun}
}



DebugStartup
