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
    variable SearchDir               "forwards"
    variable BoundaryPolicy          "any_bounds"
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


    # Creates SearchState and initiates search
    # Returns whatever FindItI2 returns
    proc FindItInner {text} {
        variable SearchDir
        variable SearchString
        variable findcase
        variable SearchState
        variable BoundaryPolicy

        set SearchState [dict create                           \
                             -widgetWhereSought $text          \
                             -startFrom         insert         \
                             -direction         $SearchDir     \
                             -searchStringQ     $SearchString  \
                             -findcase          $findcase      \
                             -BoundaryPolicy    $BoundaryPolicy \
                             -ReplaceCanceled   0               \
                        ]
                        
        return [FindItI2 $text]
    }
    

    # Find command 
    proc FindIt {text} {
        set found [FindItInner $text]
        ProcessFindItResult $found 
    }


    proc CheckCharAgainstBoundaryPolicy {char policy} {
        if {$policy eq "lisp_identifier"} {
            return [regexp [::tkcon::BoundOfLispSymbolRegexpSimplified] $char]
        } else {
            return 1
        }
    }

    # If location of SearchString found at FoundPos matches boundary policy, returns 1. Otherwise, returns 0
    proc CheckFoundMatchAgainstBoundaryPolicy {text FoundPos} {
        variable SearchState

        if {$FoundPos eq {}} {
            return 0
        }

        set SearchString [dict get $SearchState -searchStringQ]
        set SearchDir [dict get $SearchState -direction]
        set findcase [dict get $SearchState -findcase]
        set BoundaryPolicy [dict get $SearchState -BoundaryPolicy]
        
        if {$FoundPos eq "1.0"} {
            set MatchesAtLeft 1
        } else {
            set leftChar [$text get [string cat $FoundPos "-1c"]]
            set MatchesAtLeft [CheckCharAgainstBoundaryPolicy $leftChar $BoundaryPolicy]
        }

        set RightIndex [$text index [string cat $FoundPos "+" [string length $SearchString] "c"]]        

        if {$RightIndex eq [$text index end]} {
            set MatchesAtRight 1
        } else {
            set rightChar [$text get $RightIndex]
            set MatchesAtRight [CheckCharAgainstBoundaryPolicy $rightChar $BoundaryPolicy]
        }

        
        if {$MatchesAtLeft && $MatchesAtRight} {
            return 1
        } else {
            return 0
        }
    }
    


    # Returns:
    # 0 if not found, 1 if found, -1 if empty search string
    # Side effects: moves insert, changes selection, changes SearchState variable
    # ContinueP is irrelevant for text search, but we insert
    # ContinueP 1 in resulting searchstate.
    proc FindItI2 {text} {
        variable SearchState

        set SearchPos [dict get $SearchState -startFrom]
        set SearchString [dict get $SearchState -searchStringQ]
        set SearchDir [dict get $SearchState -direction]
        set findcase [dict get $SearchState -findcase]

        if {$SearchString eq ""} {
            # dict set $SearchState -NoMoreMatches 1
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

        while {1==1} {        
            set SearchPos [ $text search $caset -$SearchDir -- $SearchString $SearchPos $limit]
            if {$SearchPos == ""} {
                break
            }
            if {[CheckFoundMatchAgainstBoundaryPolicy $text $SearchPos]} {
                break
            } 
            if {$SearchDir == "forwards"} {
                set delta "+1c"
            } else {
                set delta "-1c"
            }
            set SearchPos [string cat $SearchPos $delta]
        }
        
        if {$SearchPos != ""} {
            $text see $SearchPos

            $text tag remove sel 1.0 end

            if {$SearchDir == "forwards"} {
                tkTextSetCursor $text "$SearchPos+$reng char"        
            } else { tkTextSetCursor $text $SearchPos }

            $text tag add sel $SearchPos  "$SearchPos+$leng char"

            dict set SearchState -continueP 1
            dict set SearchState -startFrom $SearchPos

            return 1

        } else {
            # dict set $SearchState -NoMoreMatches 1
            return 0
        }

    }


    # Searches once. If object is found, queries replace and accomplishes it. 
    # Returns 1 if we have to can continue search and relpace, 0 otherwise. 
    # That is, if text if not found or we canceled multiple replace sequence, returns 0
    proc ReplaceIt {text} {
        variable SearchString
        variable SearchDir
        variable ReplaceString
        variable findcase
        variable rconfirm
        variable SearchState

        dict set $SearchState -ReplaceCanceled 0

        set leng [string length $SearchString]
		set reng [string length $ReplaceString]
        set found [FindItInner $text] 

        set SearchPos [dict get $SearchState -startFrom]

        ProcessFindItResult $found 

        if {$found != 1} {
            return 0
        }
        
        ::mprs::AssertEq [expr {$SearchPos ne ""}] 1

        $text see $SearchPos

        if {$rconfirm==1} { 
            $text tag add sel $SearchPos  "$SearchPos+$leng char"
            set reply [YesNoCancel "Replace" "Replace this match?" $text]
            switch -exact $reply {
                yes { 
                    $text delete $SearchPos "$SearchPos+$leng char"
                    $text insert $SearchPos $ReplaceString
                    $text tag remove sel $SearchPos
                }
                no {
                    $text tag remove sel $SearchPos
                }
                cancel {
                    $text tag remove sel $SearchPos
                    dict set $SearchState -ReplaceCanceled 1
                    return 0
                }
            }
        } else {
            $text delete $SearchPos "$SearchPos+$leng char"
            $text insert $SearchPos $ReplaceString
        }
            

        if {$SearchDir == "forwards"} {
            tkTextSetCursor $text "$SearchPos+$leng char"        
        } else {
            tkTextSetCursor $text $SearchPos 
        }
        return 1
    }

    # Was not updated to namespace. Unlikely to work
    proc ReplaceAll {text} {
        variable SearchState

        set shouldContinue [ReplaceIt $text]

        while {$shouldContinue == 1} {

            if {[dict get $SearchState -ReplaceCanceled]} {
                break
            }

            set shouldContinue [ReplaceIt $text]
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
             -BoundaryPolicy        "any_bounds"  ]
        
        ::srchtblst::TreeSearchText $tablelist $EnsurePopulatedCmd {
            ::fndrpl::ProcessFindItResult $found 
        }
    }
}
