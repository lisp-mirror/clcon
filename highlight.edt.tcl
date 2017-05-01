# Processing highlights from oduvanchik

namespace eval ::edt {
    variable ColorTable
    variable NumberOfPendingHighlights 0

    # See also odu::*open-paren-highlight-font*
    variable OpenParenHighlightFont 10
    variable SHriftLeksicheskikhOshibok 15


    # see oi::state-font . To show pallette, call ::edt::DisplayTestTextInColor red
    # No error having two "light coral" - s - one of them has other background
    set ColorTable [ExtractValuesFromNumberedInitializator {
       0 black
       1 "dark blue"
       2 firebrick
       3 gold
       4 forestgreen   
       5 blue 
       6 orchid        
       7 rosybrown     
       8 "saddle brown"
       9 darkgoldenrod        
       10 {light coral}
       11 "cyan"        
       12 "medium orchid"    
       13 "medium purple"   
       14 {light coral}
       15 purple           
       16 "dark grey"       
    }
    ]

    proc DisplayTestTextInColor {auxColor} {
        variable ColorTable
        variable ::tkcon::OPT
        set tl [string cat .displayTestTextInColor [GenNamedCounter DisplayTestTextInColor]]
        toplevel $tl
        set i 0
        set ct $ColorTable
        lappend ct $auxColor
        puts $ct
        foreach color [concat $ColorTable [list $auxColor]] {
          set txt $tl.text$i
          text $txt
          $txt configure -foreground $color -height 1 -font $OPT(font)
          $txt insert 1.0 "$i $color"
          grid $txt 
          incr i
        }
        focus $tl
    }

    proc HighlightTagName {i} {
        string cat "hghl" $i
    }
    
    proc CreateHighlightTags {text} {
        variable ColorTable
        variable OpenParenHighlightFont
        variable SHriftLeksicheskikhOshibok
        set i 0
        foreach e $ColorTable {
            set TagName [HighlightTagName $i]
            $text tag configure $TagName -foreground $e
            incr i
        }
        # Некоторые шрифты имеют ещё и фон... 
        $text tag configure [HighlightTagName $OpenParenHighlightFont] -background green
        $text tag configure [HighlightTagName $SHriftLeksicheskikhOshibok] -background forestgreen
    }

    proc ApplyHighlightToLineSegment {text OldCharPos CharPos OldFont LineNumber} {
        set i1 [string cat $LineNumber "." $OldCharPos]
        set i2 [string cat $LineNumber "." $CharPos]
        set TagName [HighlightTagName $OldFont]
        $text tag add $TagName $i1 $i2
    }

    proc DeleteHighlightInLine {text LineNumber} {
        variable ColorTable
        set i 0
        foreach e $ColorTable {
            set TagName [HighlightTagName $i]
            set i1 [string cat $LineNumber ".0"]
            set i2 [string cat $LineNumber ".end"]
            $text tag remove $TagName $i1 $i2
            incr i
        }
    }

    # "старая" раскраска, для лиспа.
    # ::edt::ApplyHighlight3 - "новая" раскраска, для Яра
    proc ApplyHighlightToLine {clcon_text s} {
        if {[winfo exists $clcon_text]} {
            variable NumberOfPendingHighlights
            incr NumberOfPendingHighlights
            after idle [list ::edt::DoApplyHighlightToLine $clcon_text $s]
        }
    }

    # Display change of current package in a status bar
    # see also ::tkcon::ChangeCurrentPackageA, ::edt::CurrentReadtableChange
    proc CurrentPackageChange {clcon_text data} {
        global $clcon_text.StatusBarInfo
        if {[lindex $data 0]} {
            set $clcon_text.StatusBarInfo(Package) [lindex $data 1]
        } else {
            set $clcon_text.StatusBarInfo(Package) {CL-USER}
        }
    }


    # Clone of ::edt::CurrentPackageChange
    proc CurrentReadtableChange {clcon_text data} {
        global $clcon_text.StatusBarInfo
        if {[lindex $data 0]} {
            set $clcon_text.StatusBarInfo(Readtable) [lindex $data 1]
        } else {
            set $clcon_text.StatusBarInfo(Readtable) {??UNKNOWN??}
        }
    }

    # Clone of ::edt::CurrentPackageChange
    proc CurrentModeChange {clcon_text data} {
        global $clcon_text.StatusBarInfo
        if {[lindex $data 0]} {
            set $clcon_text.StatusBarInfo(Mode) [lindex $data 1]
        } else {
            set $clcon_text.StatusBarInfo(Mode) {?UNKNOWN?}
        }
    }


    proc DoApplyHighlightToLine {clcon_text s} {
        # text could disappear
        if {[winfo exists $clcon_text]} {
            DoApplyHighlightToLineInner $clcon_text $s
        }
        incr NumberOfPendingHighlights -1
    }   
    
    # Accepts string encoded by odu::encode-marks-for-line
    proc DoApplyHighlightToLineInner {clcon_text s} {
        variable NumberOfPendingHighlights
        set i 0
        foreach e $s {
            if {$i == 0} {
                set LineNumber $e
                DeleteHighlightInLine $clcon_text $LineNumber
                set PrevCharPos 0
                set PrevFont 0
            } else {
                set CharPos [lindex $e 0]
                set Font [lindex $e 1]
                ApplyHighlightToLineSegment $clcon_text $PrevCharPos $CharPos $PrevFont $LineNumber
                set PrevCharPos $CharPos
                set PrevFont $Font
            }
            incr i
        }
        ApplyHighlightToLineSegment $clcon_text $PrevCharPos "end" $PrevFont $LineNumber
    }
}


