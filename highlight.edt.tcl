# Processing highlights from oduvanchik

namespace eval ::edt {
    variable ColorTable

    set ColorTable {black blue red yellow orange
        blue blue blue blue
        blue blue blue blue
        blue blue blue blue
        blue blue blue blue
        blue blue blue blue
        blue blue blue blue
    }

    proc HighlightTagName {i} {
        string cat "hghl" $i
    }
    
    proc CreateHighlightTags {text} {
        variable ColorTable
        set i 0
        foreach e $ColorTable {
            set TagName [HighlightTagName $i]
            $text tag configure $TagName -foreground $e
            incr i
        }
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
    
    # Accepts string encoded by odu::encode-marks-for-line
    proc ApplyHighlightToLine {text s} {
        set i 0
        foreach e $s {
            if {$i == 0} {
                set LineNumber $e
                DeleteHighlightInLine $text $LineNumber
                set PrevCharPos 0
                set PrevFont 0
            } else {
                set CharPos [lindex $e 0]
                set Font [lindex $e 1]
                ApplyHighlightToLineSegment $text $PrevCharPos $CharPos $PrevFont $LineNumber
                set PrevCharPos $CharPos
                set PrevFont $Font
            }
            incr i
        }
        ApplyHighlightToLineSegment $text $PrevCharPos "end" $PrevFont $LineNumber
    }
}
