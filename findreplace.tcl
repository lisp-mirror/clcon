# This code is from tcltextedit,
#
#? Created by Dennis Ehlin
#? mail: hkc141i@tninet.se
#? http://www.user.tninet.se/~hkc141i
#? Modified by Denis Budyak, (c) 2015
#?
#? Related topics: %lDisclaimer% %lPromotion% 
#?
#?-Promotion
#?
#? Please feel free to use this logo at your homepage
#?
#? %pttenow.gif%
#?
#? Just copy the image "ttenow.gif " and insert the html code below where you want it
#? The image is included in the distribution. 
#?
#? <A HREF="http://user.tninet.se/~hkc141i/edit.html">
#? <IMG SRC="ttenow.gif" BORDER="0"
#? ALT="Get TCL TextEdit now!!"></A>
#?
#?-Disclaimer
#?
#?%2TCL TextEdit v0.9.x%
#?
#?Copyright (c) 1997-1999 Dennis Ehlin
#?Copyright (c) 2015 Denis Budyak
#?
#?Permission is hereby granted, free of charge, to any person obtaining a copy
#?of this software and associated documentation files, to deal
#?in the Software without restriction, including without limitation the rights
#?to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#?copies of the Software, and to permit persons to whom the Software is
#?furnished to do so, subject to the following conditions:
#?
#?The above copyright notice and this permission notice shall be included in
#?all copies or substantial portions of the Software.
#?
#?THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#?IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#?FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
#?AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#?LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#?OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
#?THE SOFTWARE
#?
#
# Current state
# Find: works for text and tablelist
# Find next: seem to work
# Grep: untested, must not work, but code is kept for a future
# Replace: untested, must not work, but code is kept for a future

namespace eval ::fndrpl {
    # These are variables for a dialog window. Move to separate namespace? 
    variable glb
    # variable c

    # used in grep
    variable window 

    variable SearchString            ""
    variable SearchPos               "1.0"
    variable SearchDir               "forwards"
    variable findcase                0

    # used in grep
    variable current_window          # 1
    
    variable greps
    variable r
    variable ReplaceString           ""
    variable rconfirm

    # This variable is set to 1 when dialog appears and when
    # some parameters (excluding direction) are modified by a user.
    # Before the start of the search, tts value is set to continueP
    # field of SearchState 
    
    variable SearchParamsChanged     1

    # SearchState may contain a copies of data from dialog. It is related to search process itself, not to a dialog box. 
    variable SearchState [dict create -continueP 0 -direction "forwards" -findcase 0 -searchStringQ {}]
    # Other necessary elements of SearchState are:
    #     -widgetWhereSought                   search area (tablelist or text)
    #     -startFrom                           index


    proc WhenSomeOfSearchVariablesChanged {name1 name2 op} {
        variable SearchParamsChanged 
        set SearchParamsChanged 1
    }
    
    trace add variable SearchString write ::fndrpl::WhenSomeOfSearchVariablesChanged
    trace add variable findcase write ::fndrpl::WhenSomeOfSearchVariablesChanged

    proc greplist { text greps } {
        variable glb
        variable window

        set glb .glb
        catch { destroy $glb }

        toplevel $glb
        wm title $glb "Grep results"                

        frame $glb.container  -borderwidth 0

        scrollbar $glb.vsb -orient vertical -command [list $glb.listbox yview]
        scrollbar $glb.hsb -orient horizontal -command [list $glb.listbox xview]

        mclistbox::mclistbox $glb.listbox \
            -bd 0 \
            -height 10 \
            -width 60 \
            -columnrelief flat \
            -labelanchor w \
            -columnborderwidth 0 \
            -selectmode extended \
            -labelborderwidth 1 \
            -fillcolumn file \
            -xscrollcommand [list $glb.hsb set] \
            -yscrollcommand [list $glb.vsb set] \
            -background $c(color-editbg) -borderwidth 0



        # add the columns we want to see
        $glb.listbox column add file -label "File"          -width 20
        $glb.listbox column add  text -label "Text"          -width 40

        grid $glb.vsb -in $glb.container -row 0 -column 1 -sticky ns
        grid $glb.hsb -in $glb.container -row 1 -column 0 -sticky ew
        grid $glb.listbox -in $glb.container -row 0 -column 0 -sticky nsew -padx 0 -pady 0
        grid columnconfigure $glb.container 0 -weight 1
        grid columnconfigure $glb.container 1 -weight 0
        grid rowconfigure    $glb.container 0 -weight 1
        grid rowconfigure    $glb.container 1 -weight 0

        pack $glb.container -side top -fill both -expand y

        set i 0
        while {$i<=[llength $greps]} {
            set win [lindex $greps $i]
            incr i
            incr i
            if {$win!=""} {  $glb.listbox insert end "{[win::pton $window($win,name)]} [lindex $greps $i]" }
            incr i
        }

        bind $glb.listbox <Double-Button-1> {
            variable glb
            set po [$glb.listbox curselection]
            set win [lindex $greps [expr $po * 3]]
            set pos [lindex $greps [expr $po * 3+1]]
            set str [lindex $greps [expr $po * 3+2]]
            
            win::activate $win
            $text see $pos
            $text tag delete sel
            tkTextSetCursor $text $pos
            $text tag add sel $pos "$pos + [string length $str] char"
        }
    }

    # Was not updated to namespace. Unlikely to work
    proc GrepIt {text} {
        variable SearchString
        variable SearchPos
        variable SearchDir
        variable findcase
        variable current_window
        variable window
        variable glb
        variable greps

        c $SearchString $SearchPos $SearchDir $findcase

        set greps ""

        if {$SearchString!=""} {
            if {$findcase=="1"} {
                set caset "-exact"
            } else {
                set caset "-nocase"
            }
            set limit end

            set cont 1
            set startw $current_window
            set i 0

            while {$cont==1} {
                set SearchPos "1.0"


                set found 1 
                while {$found==1} {

                    set leng [string length $SearchString]
                    set SearchPos [ $text search $caset -$SearchDir -- $SearchString $SearchPos $limit]
                    if {$SearchPos != ""} {
                        set sta "[lindex [split $SearchPos {.}] 0].0"

                        set str [list [string trim [$text get $sta "$sta+1 line"] "\n" ] ]
                        set greps "$greps $current_window $sta {$str} "


                        if {$SearchDir == "forwards"} {
                            set SearchPos "$SearchPos+$leng char"
                        }         
                    } else {
                        set found 0
                    }
                }

                win::Next
                if {$current_window==$startw} {set cont 0}

            } 

        }
        greplist $text $greps
    }


    ################################# Find proc #################################

    proc ProcessFindItResult {found} {
        variable SearchState
        set widgetWhereSought [dict get $SearchState -widgetWhereSought]
        if {$found==1} {
            return
        } elseif {$found==-1} {
            tk_messageBox -parent $widgetWhereSought -title "Find" \
                -message "Empty search string"
        } else {
            tk_messageBox -parent $widgetWhereSought -title "Find" \
                -message "Search string not found"
        }
    }

    # Entry from FindBox dialog box to search process 
    proc FindIt {text} {
        variable SearchDir
        variable SearchString
        variable findcase
        variable SearchState

        set SearchState [dict create                       \
                             -widgetWhereSought $text      \
                             -startFrom     insert         \
                             -direction     $SearchDir     \
                             -searchStringQ $SearchString  \
                             -findcase      $findcase      \
                        ]

        set found [FindItInner $text]

        ProcessFindItResult $found 
        
    }

    # Returns two values:
    # 1) 0 if not found, 1 if found, -1 if empty search string
    # 2) new SearchState
    # Side effects: moves insert, changes selection
    # ContinueP is irrelevant for text search, but we insert
    # ContinueP 1 in resulting searchstate.
    proc FindItInner {text} {
        variable SearchState

        set SearchPos [dict get $SearchState -startFrom]
        set SearchString [dict get $SearchState -searchStringQ]
        set SearchDir [dict get $SearchState -direction]
        set findcase [dict get $SearchState -findcase]

        if {$SearchString eq ""} {
            return -1
        }
        if {$findcase=="1"} {
            set caset "-exact"
        } else {
            set caset "-nocase"
        }
            
        if {$SearchDir == "forwards"} {
            set limit end
        } else {
            set limit 1.0
        }

        set leng [string length $SearchString]

        set SearchPos [ $text search $caset -$SearchDir -- $SearchString $SearchPos $limit]

        if {$SearchPos != ""} {
            $text see $SearchPos

            $text tag remove sel 1.0 end

            if {$SearchDir == "forwards"} {
                tkTextSetCursor $text "$SearchPos+$leng char"        
            } else { tkTextSetCursor $text $SearchPos }

            $text tag add sel $SearchPos  "$SearchPos+$leng char"

            dict set SearchState -continueP 1
            dict set SearchState -startFrom $SearchPos
            return 1

        } else {
            return 0
        }

    }


    ## OldTkconFind - code from tkcon searches in text widget $w for $str and highlights it
    ## If $str is empty, it just deletes any highlighting
    # ARGS: w	- text widget
    #	str	- string to search for
    #	-case	TCL_BOOLEAN	whether to be case sensitive	DEFAULT: 0
    #	-regexp	TCL_BOOLEAN	whether to use $str as pattern	DEFAULT: 0
    ##
    proc OldTkconFind {w str args} {
        $w tag remove find 1.0 end
        set truth {^(1|yes|true|on)$}
        set opts  {}
        foreach {key val} $args {
            switch -glob -- $key {
                -c* { if {[regexp -nocase $truth $val]} { set case 1 } }
                -r* { if {[regexp -nocase $truth $val]} { lappend opts -regexp } }
                default { return -code error "Unknown option $key" }
            }
        }
        if {![info exists case]} { lappend opts -nocase }
        if {$str eq ""} { return }
        $w mark set findmark 1.0
        # set InternalWidget [RoTextGetInternalWidget $w]
        set InternalWidget $w
        while {[set ix [eval $InternalWidget search $opts -count numc -- \
                            $str findmark end]] ne ""} {
            $w tag add find $ix ${ix}+${numc}c
            $w mark set findmark ${ix}+1c
        }
        $w tag configure find -background $::tkcon::COLOR(blink)
        catch {$w see find.first}
        return [expr {[llength [$w tag ranges find]]/2}]
    }

    
    proc question {text} {
        variable r
        set q .question
        set r "0"
        catch {destroy $q}
        toplevel $q
        wm title $q "Replace ?"
        label $q.label -text "Replace this occurance ?"
        frame $q.buttons
        button $q.buttons.yes -text "Yes" -command "set r 1" 
        button $q.buttons.no  -text "No" -command  "set r 0"
        pack $q.buttons.yes $q.buttons.no -side left
        pack $q.label $q.buttons
        powin $q $text
        grab $q
        vwait r
        destroy $q
        return $r
    }



    # Was not updated to namespace. Unlikely to work
    proc ReplaceIt {text n} {
        variable SearchString
        variable SearchDir
        variable ReplaceString
        variable findcase
        variable window
        variable current_window
        variable rconfirm

        set SearchPos insert
        c 
        set window($current_window,echange) 1
        set window($current_window,change) 1

        if {$findcase=="1"} {
            set caset "-exact"
        } else {
            set caset "-nocase"
        }
        

        if {$SearchDir == "forwards"} {
            set limit end
        } else {
            set limit 1.0
        }

        set leng [string length $SearchString]
        set SearchPos [ $text search $caset -$SearchDir -- $SearchString $SearchPos $limit]

        if {$SearchPos != ""} {
            $text see $SearchPos

            if {$rconfirm==1} { 
                $text tag add sel $SearchPos  "$SearchPos+$leng char"
                if {[question $text]==1} {
                    $text delete $SearchPos "$SearchPos+$leng char"
                    $text insert $SearchPos $ReplaceString
                }
                $text tag remove sel $SearchPos
            } else {
                $text delete $SearchPos "$SearchPos+$leng char"
                $text insert $SearchPos $ReplaceString
            }
            

            if {$SearchDir == "forwards"} {
                tkTextSetCursor $text "$SearchPos+$leng char"        
            } else { tkTextSetCursor $text $SearchPos }

        } else {
            set SearchPos "1.0"
        }
    }

    # Was not updated to namespace. Unlikely to work
    proc ReplaceAll {text} {
        variable SearchString
        variable SearchDir
        variable ReplaceString
        variable findcase
        variable window
        variable current_window

        set window($current_window,echange) 1
        set window($current_window,change) 1

        c
        ReplaceIt $text norec
        while {$SearchPos!="1.0"} {
            ReplaceIt $text norec
        }
    }

    proc CancelFind {text w} {
        $text tag delete tg1
        destroy $w
    }


    ### Proc called by "Search/Goto line" menu
    proc Goto_line_ask {text} {
        #variable c

        set gow .goto_line
        catch {destroy $gow}
        toplevel $gow
        wm title $gow "Where ?"

        frame $gow.fr1
        frame $gow.fr2
        pack $gow.fr1 $gow.fr2 -side left

        label $gow.fr1.lab -text "Goto line number:"
        entry $gow.fr1.ent 
        pack $gow.fr1.lab $gow.fr1.ent -side left -pady 10 -padx 10

        button $gow.fr2.ok -text Ok -width 10 -command "Goto_line $text $gow"
        button $gow.fr2.can -text Cancel -width 10 -command "focus $text;destroy $gow"
        pack $gow.fr2.ok $gow.fr2.can -side top -padx 10 -pady 5

        focus $gow.fr1.ent

        bind $gow.fr1.ent <Return> "Goto_line $text $gow"
        bind $gow.fr1.ent <KP_Enter> "Goto_line $gow"
        bind $gow.fr1.ent <Escape> "focus $text ; destroy $gow"
        powin $gow $text
        grab $gow
    }                                          


    proc Goto_line {text w} {
        set value [$w.fr1.ent get]
        if {[catch {expr $value} err]} {
            return }
        tkTextSetCursor $text $value.0
        ### Proc to update line number in status line
        #win::updateln
        focus $text
        destroy $w
    }

    # Entry from FindBox to tree search process, bound to button
    proc TreeSearchTextOuter {tablelist EnsurePopulatedCmd} {
        variable SearchString
        variable SearchDir
        variable findcase
        variable SearchState
        variable SearchParamsChanged

        set searchStringQ [QuoteStringForRegexp $SearchString]
        set NewContinueP [expr {1 - $SearchParamsChanged}]

        set SearchParamsChanged 0

        set SearchState                                            \
            [dict create                                           \
             -widgetWhereSought     $tablelist                     \
             -continueP             $NewContinueP                  \
             -startFrom             [$tablelist index active]      \
             -direction             $SearchDir                     \
             -searchStringQ         $searchStringQ                 \
             -findcase              $findcase                      \
            ]
        
        ::srchtblst::TreeSearchText $tablelist $EnsurePopulatedCmd {
            ::fndrpl::ProcessFindItResult $found 
        }
    }
}
