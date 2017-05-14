# Processing highlights from oduvanchik

namespace eval ::edt {
    variable ColorTable
    variable NumberOfPendingHighlights 0

    # See also odu::*open-paren-highlight-font*
    variable OpenParenHighlightFont 10
    variable SHriftLeksicheskikhOshibok 15

    variable *TK-Число-слоёв-раскраски* 6


    # see oi::state-font . To show pallette, call ::edt::DisplayTestTextInColor {red blue}
    set ColorTable [ExtractValuesFromNumberedInitializator {
       0 {black                  white}
       1 {"dark blue"            white}
       2 {firebrick              white}
       3 {gold                   white}
       4 {forestgreen            white}
       5 {blue                   white}
       6 {orchid                 white}
       7 {rosybrown              white}
       8 {"saddle brown"         white}
       9 {darkgoldenrod          white}
       10 {"light coral"         green}
       11 {"cyan"                white}
       12 {"medium orchid"       white}
       13 {"medium purple"       white}
       14 {"light coral"         white}
       15 {purple                forestgreen}
       16 {"dark grey"           white}
       17 {white                 black}
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
        foreach e [concat $ColorTable [list $auxColor]] {
          set txt $tl.text$i
          text $txt
          foreach {f b} $e break
          $txt configure -foreground $f -background $b -height 1 -font $OPT(font)
          $txt insert 1.0 "$i $f on $b"
          grid $txt 
          incr i
        }
        focus $tl
    }

    proc HighlightTagName {slojj i} {
        string cat "hghl" $slojj "_" $i
    }
    
    proc CreateHighlightTags {text} {
        variable ColorTable
        variable {*TK-Число-слоёв-раскраски*}
        for {set slojj 0} {$slojj < ${*TK-Число-слоёв-раскраски*}} {incr slojj} {
            set i 0
            foreach e $ColorTable {
                set TagName [HighlightTagName $slojj $i]
                foreach {f b} $e {
                    $text tag configure $TagName -foreground $f -background $b
                    # puts "$text tag configure $TagName -foreground $f -background $b"
                }
                incr i
            }
        }
        # Настраиваем отдельные виджеты
        $text tag bind [HighlightTagName 2 17] <Enter> {puts "Неверный отступ. Отступ должен быть таким, как размер заливки. Если залита только первая позиция строки, отступ должен быть равен нулю (или меньше нуля ввиду ошибок в предыдущих строках)"}
    }

    proc ApplyHighlightToLineSegment {text OldCharPos CharPos OldFont LineNumber} {
        set i1 [string cat $LineNumber "." $OldCharPos]
        set i2 [string cat $LineNumber "." $CharPos]
        set TagName [HighlightTagName 0 $OldFont]
        $text tag add $TagName $i1 $i2
    }

    proc DeleteHighlightInLine {text LineNumber} {
        variable ColorTable
        set i 0
        foreach e $ColorTable {
            set TagName [HighlightTagName 0 $i]
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


