## This is just pure tcl/tk application
## 

package require tablelist


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




proc InsertNewText {i} {
    global tv
    EnsureTextView
    $tv.body.text delete 0.0 end
    $tv.body.text insert end $i
    event generate $tv <<GoToTop>>
}


proc DoOnSelect {tbl text} {
    InsertNewText [string cat "Active index: " [$tbl index active] "\nThis is a error detail window. It is filled with current error details as user browses through error list. Also there will be 'goto source' hotkey\n"  $text "\nFIXME sort of messages\n"]
    # return -code continue
}

# This is a contiuation assigned on reply on initialization request 
proc SwankBrowseErrors1 { EventAsList } {
    global tv
    set w [PrepareGui1]
    EnsureTextView
    bind $w.tf.tbl <<TablelistSelect>> [list DoOnSelect $w.tf.tbl "TableListSelect fired"]
    DoOnSelect $w.tf.tbl "Initial data"
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

# --------------------------------- frames-----------------              

# paned window

    frame $w.tf
    set tbl $w.tf.tbl
    
    tablelist::tablelist $tbl -columns {20 "Compiler notes"} -stretch all -spacing 10

    $tbl configure \
        -foreground \#000000 \
        -font tkconfixed -borderwidth 1 -highlightthickness 0
        

    $tbl columnconfigure 0 -wrap true  
    
    $tbl insert end [list "error 1"]
    $tbl insert end [list "one more error"]
    $tbl insert end [list "such a long error: sadfffffffffffffffffffffffffffffffffffffffffffffffffffjkljfds djsflklk"]    

    set f1 $w.tf
    scrollbar $f1.sy -orient v -command [list $tbl yview]
    $tbl configure -yscrollcommand [list $f1.sy set]
    grid $tbl - $f1.sy -sticky news
    grid columnconfigure $f1 0 -weight 1
    grid columnconfigure $f1 1 -weight 1
    grid rowconfigure $f1 0 -weight 1

    pack $f1 -side top -fill x

    # other frame and text

    focus $tbl
    
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