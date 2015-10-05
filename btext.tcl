# By George Peter Staplin
# Changed by budden to remove highlighting. ctext renamed to btext. 
# See also the README at ctext.tcl for a list of contributors

package require Tk

namespace eval btext {}

#win is used as a unique token to create arrays for each btext instance
proc btext::getAr {win suffix name} {
    set arName __btext[set win][set suffix]
    uplevel [list upvar \#0 $arName $name]
    return $arName
}

proc btext {win args} {
    if {[llength $args] & 1} {
	return -code error \
	    "invalid number of arguments given to btext (uneven number after window) : $args"
    }

    frame $win -class Btext

    set tmp [text .__btextTemp]

    btext::getAr $win config ar

    set ar(-fg) [$tmp cget -foreground]
    set ar(-bg) [$tmp cget -background]
    set ar(-font) [$tmp cget -font]
    set ar(-relief) [$tmp cget -relief]
    destroy $tmp
    set ar(-yscrollcommand) ""
    set ar(-linemap) 1
    set ar(-linemapfg) $ar(-fg)
    set ar(-linemapbg) $ar(-bg)
    set ar(-linemap_mark_command) {}
    set ar(-linemap_markable) 1
    set ar(-linemap_select_fg) black
    set ar(-linemap_select_bg) yellow
    set ar(-highlight) 1
    set ar(win) $win
    set ar(modified) 0
    set ar(commentsAfterId) ""
    set ar(highlightAfterId) ""
    set ar(blinkAfterId) ""

    set ar(btextFlags) [list -yscrollcommand -linemap -linemapfg -linemapbg \
			    -font -linemap_mark_command -highlight -linemap_markable \
			    -linemap_select_fg \
			    -linemap_select_bg]

    array set ar $args

    foreach flag {foreground background} short {fg bg} {
	if {[info exists ar(-$flag)] == 1} {
	    set ar(-$short) $ar(-$flag)
	    unset ar(-$flag)
	}
    }

    # Now remove flags that will confuse text and those that need
    # modification:
    foreach arg $ar(btextFlags) {
	if {[set loc [lsearch $args $arg]] >= 0} {
	    set args [lreplace $args $loc [expr {$loc + 1}]]
	}
    }

    text $win.l -font $ar(-font) -width 1 -height 1 \
	-relief $ar(-relief) -fg $ar(-linemapfg) \
	-bg $ar(-linemapbg) -takefocus 0

    set topWin [winfo toplevel $win]
    bindtags $win.l [list $win.l $topWin all]

    if {$ar(-linemap) == 1} {
	grid $win.l -sticky ns -row 0 -column 0
    }

    set args [concat $args [list -yscrollcommand \
				[list btext::event:yscroll $win $ar(-yscrollcommand)]]]

    #escape $win, because it could have a space
    eval text \$win.t -font \$ar(-font) $args

    grid $win.t -row 0 -column 1 -sticky news
    grid rowconfigure $win 0 -weight 100
    grid columnconfigure $win 1 -weight 100

    bind $win.t <Configure> [list btext::linemapUpdate $win]
    bind $win.l <ButtonPress-1> [list btext::linemapToggleMark $win %y]
    bind $win.t <KeyRelease-Return> [list btext::linemapUpdate $win]
    rename $win __btextJunk$win
    rename $win.t $win._t

    bind $win <Destroy> [list btext::event:Destroy $win %W]
    bindtags $win.t [linsert [bindtags $win.t] 0 $win]

    interp alias {} $win {} btext::instanceCmd $win
    interp alias {} $win.t {} $win

    # If the user wants C comments they should call
    # btext::enableComments
    btext::disableComments $win
    btext::modified $win 0
    btext::buildArgParseTable $win

    return $win
}

proc btext::event:yscroll {win clientData args} {
    btext::linemapUpdate $win

    if {$clientData == ""} {
	return
    }
    uplevel \#0 $clientData $args
}

proc btext::event:Destroy {win dWin} {
    if {![string equal $win $dWin]} {
	return
    }

    btext::getAr $win config configAr

    catch {after cancel $configAr(commentsAfterId)}
    catch {after cancel $configAr(highlightAfterId)}
    catch {after cancel $configAr(blinkAfterId)}

    catch {rename $win {}}
    interp alias {} $win.t {}
    btext::clearHighlightClasses $win
    array unset [btext::getAr $win config ar]
}

# This stores the arg table within the config array for each instance.
# It's used by the configure instance command.
proc btext::buildArgParseTable win {
    set argTable [list]

    lappend argTable any -linemap_mark_command {
	set configAr(-linemap_mark_command) $value
	break
    }

    lappend argTable {1 true yes} -linemap {
	grid $self.l -sticky ns -row 0 -column 0
	grid columnconfigure $self 0 \
	    -minsize [winfo reqwidth $self.l]
	set configAr(-linemap) 1
	break
    }

    lappend argTable {0 false no} -linemap {
	grid forget $self.l
	grid columnconfigure $self 0 -minsize 0
	set configAr(-linemap) 0
	break
    }

    lappend argTable any -yscrollcommand {
	set cmd [list $self._t config -yscrollcommand \
		     [list btext::event:yscroll $self $value]]

	if {[catch $cmd res]} {
	    return $res
	}
	set configAr(-yscrollcommand) $value
	break
    }

    lappend argTable any -linemapfg {
	if {[catch {winfo rgb $self $value} res]} {
	    return -code error $res
	}
	$self.l config -fg $value
	set configAr(-linemapfg) $value
	break
    }

    lappend argTable any -linemapbg {
	if {[catch {winfo rgb $self $value} res]} {
	    return -code error $res
	}
	$self.l config -bg $value
	set configAr(-linemapbg) $value
	break
    }

    lappend argTable any -font {
	if {[catch {$self.l config -font $value} res]} {
	    return -code error $res
	}
	$self._t config -font $value
	set configAr(-font) $value
	break
    }

    lappend argTable {0 false no} -highlight {
	set configAr(-highlight) 0
	break
    }

    lappend argTable {1 true yes} -highlight {
	set configAr(-highlight) 1
	break
    }

    lappend argTable {0 false no} -linemap_markable {
	set configAr(-linemap_markable) 0
	break
    }

    lappend argTable {1 true yes} -linemap_markable {
	set configAr(-linemap_markable) 1
	break
    }

    lappend argTable any -linemap_select_fg {
	if {[catch {winfo rgb $self $value} res]} {
	    return -code error $res
	}
	set configAr(-linemap_select_fg) $value
	$self.l tag configure lmark -foreground $value
	break
    }

    lappend argTable any -linemap_select_bg {
	if {[catch {winfo rgb $self $value} res]} {
	    return -code error $res
	}
	set configAr(-linemap_select_bg) $value
	$self.l tag configure lmark -background $value
	break
    }

    btext::getAr $win config ar
    set ar(argTable) $argTable
}

proc btext::commentsAfterIdle {win} {

    return 

    btext::getAr $win config configAr

    if {"" eq $configAr(commentsAfterId)} {
	set configAr(commentsAfterId) [after idle \
	   [list btext::comments $win [set afterTriggered 1]]]
    }
}

proc btext::highlightAfterIdle {win lineStart lineEnd} {

    return 

    btext::getAr $win config configAr

    if {"" eq $configAr(highlightAfterId)} {
	set configAr(highlightAfterId) [after idle \
	    [list btext::highlight $win $lineStart $lineEnd [set afterTriggered 1]]]
    }
}

proc btext::instanceCmd {self cmd args} {
    #slightly different than the RE used in btext::comments
    set commentRE {\"|\\|'|/|\*}

    switch -glob -- $cmd {
	append {
	    if {[catch {$self._t get sel.first sel.last} data] == 0} {
		clipboard append -displayof $self $data
	    }
	}

	cget {
	    set arg [lindex $args 0]
	    btext::getAr $self config configAr

	    foreach flag $configAr(btextFlags) {
		if {[string match ${arg}* $flag]} {
		    return [set configAr($flag)]
		}
	    }
	    return [$self._t cget $arg]
	}

	conf* {
	    btext::getAr $self config configAr

	    if {0 == [llength $args]} {
		set res [$self._t configure]
		set del [lsearch -glob $res -yscrollcommand*]
		set res [lreplace $res $del $del]
		foreach flag $configAr(btextFlags) {
		    lappend res [list $flag [set configAr($flag)]]
		}
		return $res
	    }

	    array set flags {}
	    foreach flag $configAr(btextFlags) {
		set loc [lsearch $args $flag]
		if {$loc < 0} {
		    continue
		}

		if {[llength $args] <= ($loc + 1)} {
		    #.t config -flag
		    return [set configAr($flag)]
		}

		set flagArg [lindex $args [expr {$loc + 1}]]
		set args [lreplace $args $loc [expr {$loc + 1}]]
		set flags($flag) $flagArg
	    }

	    foreach {valueList flag cmd} $configAr(argTable) {
		if {[info exists flags($flag)]} {
		    foreach valueToCheckFor $valueList {
			set value [set flags($flag)]
			if {[string equal "any" $valueToCheckFor]} $cmd \
			    elseif {[string equal $valueToCheckFor [set flags($flag)]]} $cmd
		    }
		}
	    }

	    if {[llength $args]} {
		#we take care of configure without args at the top of this branch
		uplevel 1 [linsert $args 0 $self._t configure]
	    }
	}

	copy {
	    tk_textCopy $self
	}

	cut {
	    if {[catch {$self.t get sel.first sel.last} data] == 0} {
		clipboard clear -displayof $self.t
		clipboard append -displayof $self.t $data
		$self delete [$self.t index sel.first] [$self.t index sel.last]
		btext::modified $self 1
	    }
	}

	delete {
	    #delete n.n ?n.n

	    set argsLength [llength $args]

	    #first deal with delete n.n
	    if {$argsLength == 1} {
		set deletePos [lindex $args 0]
		set prevChar [$self._t get $deletePos]

		$self._t delete $deletePos
		set char [$self._t get $deletePos]

		set prevSpace [btext::findPreviousSpace $self._t $deletePos]
		set nextSpace [btext::findNextSpace $self._t $deletePos]

		set lineStart [$self._t index "$deletePos linestart"]
		set lineEnd [$self._t index "$deletePos + 1 chars lineend"]

		#This pattern was used in 3.1.  We may want to investigate using it again
		#eventually to reduce flicker.  It caused a bug with some patterns.
		#if {[string equal $prevChar "#"] || [string equal $char "#"]} {
		#	set removeStart $lineStart
		#	set removeEnd $lineEnd
		#} else {
		#	set removeStart $prevSpace
		#	set removeEnd $nextSpace
		#}
		set removeStart $lineStart
		set removeEnd $lineEnd

		# foreach tag [$self._t tag names] {
		#     if {[string equal $tag "_cComment"] != 1} {
		# 	$self._t tag remove $tag $removeStart $removeEnd
		#     }
		# }

		# set checkStr "$prevChar[set char]"

		# if {[regexp $commentRE $checkStr]} {
		#     btext::commentsAfterIdle $self
		# }

		btext::highlightAfterIdle $self $lineStart $lineEnd
		btext::linemapUpdate $self
	    } elseif {$argsLength == 2} {
		#now deal with delete n.n ?n.n?
		set deleteStartPos [lindex $args 0]
		set deleteEndPos [lindex $args 1]

		set data [$self._t get $deleteStartPos $deleteEndPos]

		# set lineStart [$self._t index "$deleteStartPos linestart"]
		# set lineEnd [$self._t index "$deleteEndPos + 1 chars lineend"]
		# eval \$self._t delete $args

		# foreach tag [$self._t tag names] {
		#     if {[string equal $tag "_cComment"] != 1} {
		# 	$self._t tag remove $tag $lineStart $lineEnd
		#     }
		# }

		# if {[regexp $commentRE $data]} {
		#     btext::commentsAfterIdle $self
		# }

		# btext::highlightAfterIdle $self $lineStart $lineEnd
		if {[string first "\n" $data] >= 0} {
		    btext::linemapUpdate $self
		}
	    } else {
		return -code error "invalid argument(s) sent to $self delete: $args"
	    }
	    btext::modified $self 1
	}

	fastdelete {
	    eval \$self._t delete $args
	    btext::modified $self 1
	    btext::linemapUpdate $self
	}

	fastinsert {
	    eval \$self._t insert $args
	    btext::modified $self 1
	    btext::linemapUpdate $self
	}

	highlight {
	    #btext::highlight $self [lindex $args 0] [lindex $args 1]
	    #btext::comments $self
	}

	insert {
	    if {[llength $args] < 2} {
		return -code error "please use at least 2 arguments to $self insert"
	    }

	    set insertPos [lindex $args 0]
	    set prevChar [$self._t get "$insertPos - 1 chars"]
	    set nextChar [$self._t get $insertPos]
	    set lineStart [$self._t index "$insertPos linestart"]
	    set prevSpace [btext::findPreviousSpace $self._t ${insertPos}-1c]
	    set data [lindex $args 1]
	    eval \$self._t insert $args

	    # set nextSpace [btext::findNextSpace $self._t insert]
	    # set lineEnd [$self._t index "insert lineend"]

	    # if {[$self._t compare $prevSpace < $lineStart]} {
	    #     set prevSpace $lineStart
	    # }

	    # if {[$self._t compare $nextSpace > $lineEnd]} {
	    #     set nextSpace $lineEnd
	    # }

	    # foreach tag [$self._t tag names] {
	    #     if {[string equal $tag "_cComment"] != 1} {
	    #         $self._t tag remove $tag $prevSpace $nextSpace
	    #     }
	    # }

	    # set REData $prevChar
	    # append REData $data
	    # append REData $nextChar
	    # if {[regexp $commentRE $REData]} {
	    #     btext::commentsAfterIdle $self
	    # }

	    # btext::highlightAfterIdle $self $lineStart $lineEnd

	    # switch -- $data {
	    #     "\}" {
	    #         btext::matchPair $self "\\\{" "\\\}" "\\"
	    #     }
	    #     "\]" {
	    #         btext::matchPair $self "\\\[" "\\\]" "\\"
	    #     }
	    #     "\)" {
	    #         btext::matchPair $self "\\(" "\\)" ""
	    #     }
	    #     "\"" {
	    #         btext::matchQuote $self
	    #     }
	    # }
	    btext::modified $self 1
	    btext::linemapUpdate $self
	}

	paste {
	    tk_textPaste $self
	    btext::modified $self 1
	}

	edit {
	    set subCmd [lindex $args 0]
	    set argsLength [llength $args]

	    btext::getAr $self config ar

	    if {"modified" == $subCmd} {
		if {$argsLength == 1} {
		    return $ar(modified)
		} elseif {$argsLength == 2} {
		    set value [lindex $args 1]
		    set ar(modified) $value
		} else {
		    return -code error "invalid arg(s) to $self edit modified: $args"
		}
	    } else {
		#Tk 8.4 has other edit subcommands that I don't want to emulate.
		return [uplevel 1 [linsert $args 0 $self._t $cmd]]
	    }
	}

	default {
	    return [uplevel 1 [linsert $args 0 $self._t $cmd]]
	}
    }
}

proc btext::tag:blink {win count {afterTriggered 0}} {
    if {$count & 1} {
	$win tag configure __btext_blink \
	    -foreground [$win cget -bg] -background [$win cget -fg]
    } else {
	$win tag configure __btext_blink \
	    -foreground [$win cget -fg] -background [$win cget -bg]
    }

    btext::getAr $win config configAr
    if {$afterTriggered} {
	set configAr(blinkAfterId) ""
    }

    if {$count == 4} {
	$win tag delete __btext_blink 1.0 end
	return
    }

    incr count
    if {"" eq $configAr(blinkAfterId)} {
	set configAr(blinkAfterId) [after 50 \
		[list btext::tag:blink $win $count [set afterTriggered 1]]]
    }
}

proc btext::matchPair {win str1 str2 escape} {
    set prevChar [$win get "insert - 2 chars"]

    if {[string equal $prevChar $escape]} {
	#The char that we thought might be the end is actually escaped.
	return
    }

    set searchRE "[set str1]|[set str2]"
    set count 1

    set pos [$win index "insert - 1 chars"]
    set endPair $pos
    set lastFound ""
    while 1 {
	set found [$win search -backwards -regexp $searchRE $pos]

	if {$found == "" || [$win compare $found > $pos]} {
	    return
	}

	if {$lastFound != "" && [$win compare $found == $lastFound]} {
	    #The search wrapped and found the previous search
	    return
	}

	set lastFound $found
	set char [$win get $found]
	set prevChar [$win get "$found - 1 chars"]
	set pos $found

	if {[string equal $prevChar $escape]} {
	    continue
	} elseif {[string equal $char [subst $str2]]} {
	    incr count
	} elseif {[string equal $char [subst $str1]]} {
	    incr count -1
	    if {$count == 0} {
		set startPair $found
		break
	    }
	} else {
	    # This shouldn't happen.  I may in the future make it
	    # return -code error
	    puts stderr "btext seems to have encountered a bug in btext::matchPair"
	    return
	}
    }

    $win tag add __btext_blink $startPair
    $win tag add __btext_blink $endPair
    btext::tag:blink $win 0
}

proc btext::matchQuote {win} {
    set endQuote [$win index insert]
    set start [$win index "insert - 1 chars"]

    if {[$win get "$start - 1 chars"] == "\\"} {
	#the quote really isn't the end
	return
    }
    set lastFound ""
    while 1 {
	set startQuote [$win search -backwards \" $start]
	if {$startQuote == "" || [$win compare $startQuote > $start]} {
	    #The search found nothing or it wrapped.
	    return
	}

	if {$lastFound != "" && [$win compare $lastFound == $startQuote]} {
	    #We found the character we found before, so it wrapped.
	    return
	}
	set lastFound $startQuote
	set start [$win index "$startQuote - 1 chars"]
	set prevChar [$win get $start]

	if {$prevChar == "\\"} {
	    continue
	}
	break
    }

    if {[$win compare $endQuote == $startQuote]} {
	#probably just \"
	return
    }

    $win tag add __btext_blink $startQuote $endQuote
    btext::tag:blink $win 0
}

proc btext::enableComments {win} {
    $win tag configure _cComment -foreground khaki
}
proc btext::disableComments {win} {
    catch {$win tag delete _cComment}
}

proc btext::comments {win {afterTriggered 0}} {
    if {[catch {$win tag cget _cComment -foreground}]} {
	#C comments are disabled
	return
    }

    if {$afterTriggered} {
	btext::getAr $win config configAr
	set configAr(commentsAfterId) ""
    }

    set startIndex 1.0
    set commentRE {\\\\|\"|\\\"|\\'|'|/\*|\*/}
    set commentStart 0
    set isQuote 0
    set isSingleQuote 0
    set isComment 0
    $win tag remove _cComment 1.0 end
    while 1 {
	set index [$win search -count length -regexp $commentRE $startIndex end]

	if {$index == ""} {
	    break
	}

	set endIndex [$win index "$index + $length chars"]
	set str [$win get $index $endIndex]
	set startIndex $endIndex

	if {$str == "\\\\"} {
	    continue
	} elseif {$str == "\\\""} {
	    continue
	} elseif {$str == "\\'"} {
	    continue
	} elseif {$str == "\"" && $isComment == 0 && $isSingleQuote == 0} {
	    if {$isQuote} {
		set isQuote 0
	    } else {
		set isQuote 1
	    }
	} elseif {$str == "'" && $isComment == 0 && $isQuote == 0} {
	    if {$isSingleQuote} {
		set isSingleQuote 0
	    } else {
		set isSingleQuote 1
	    }
	} elseif {$str == "/*" && $isQuote == 0 && $isSingleQuote == 0} {
	    if {$isComment} {
		#comment in comment
		break
	    } else {
		set isComment 1
		set commentStart $index
	    }
	} elseif {$str == "*/" && $isQuote == 0 && $isSingleQuote == 0} {
	    if {$isComment} {
		set isComment 0
		$win tag add _cComment $commentStart $endIndex
		$win tag raise _cComment
	    } else {
		#comment end without beginning
		break
	    }
	}
    }
}

proc btext::addHighlightClass {win class color keywords} {
    set ref [btext::getAr $win highlight ar]
    foreach word $keywords {
	set ar($word) [list $class $color]
    }
    $win tag configure $class

    btext::getAr $win classes classesAr
    set classesAr($class) [list $ref $keywords]
}

#For [ ] { } # etc.
proc btext::addHighlightClassForSpecialChars {win class color chars} {
    set charList [split $chars ""]

    set ref [btext::getAr $win highlightSpecialChars ar]
    foreach char $charList {
	set ar($char) [list $class $color]
    }
    $win tag configure $class

    btext::getAr $win classes classesAr
    set classesAr($class) [list $ref $charList]
}

proc btext::addHighlightClassForRegexp {win class color re} {
    set ref [btext::getAr $win highlightRegexp ar]

    set ar($class) [list $re $color]
    $win tag configure $class

    btext::getAr $win classes classesAr
    set classesAr($class) [list $ref $class]
}

#For things like $blah
proc btext::addHighlightClassWithOnlyCharStart {win class color char} {
    set ref [btext::getAr $win highlightCharStart ar]

    set ar($char) [list $class $color]
    $win tag configure $class

    btext::getAr $win classes classesAr
    set classesAr($class) [list $ref $char]
}

proc btext::deleteHighlightClass {win classToDelete} {
    btext::getAr $win classes classesAr

    if {![info exists classesAr($classToDelete)]} {
	return -code error "$classToDelete doesn't exist"
    }

    foreach {ref keyList} [set classesAr($classToDelete)] {
	upvar #0 $ref refAr
	foreach key $keyList {
	    if {![info exists refAr($key)]} {
		continue
	    }
	    unset refAr($key)
	}
    }
    unset classesAr($classToDelete)
}

proc btext::getHighlightClasses win {
    btext::getAr $win classes classesAr

    array names classesAr
}

proc btext::findNextChar {win index char} {
    set i [$win index "$index + 1 chars"]
    set lineend [$win index "$i lineend"]
    while 1 {
	set ch [$win get $i]
	if {[$win compare $i >= $lineend]} {
	    return ""
	}
	if {$ch == $char} {
	    return $i
	}
	set i [$win index "$i + 1 chars"]
    }
}

proc btext::findNextSpace {win index} {
    set i [$win index $index]
    set lineStart [$win index "$i linestart"]
    set lineEnd [$win index "$i lineend"]
    #Sometimes the lineend fails (I don't know why), so add 1 and try again.
    if {[$win compare $lineEnd == $lineStart]} {
	set lineEnd [$win index "$i + 1 chars lineend"]
    }

    while {1} {
	set ch [$win get $i]

	if {[$win compare $i >= $lineEnd]} {
	    set i $lineEnd
	    break
	}

	if {[string is space $ch]} {
	    break
	}
	set i [$win index "$i + 1 chars"]
    }
    return $i
}

proc btext::findPreviousSpace {win index} {
    set i [$win index $index]
    set lineStart [$win index "$i linestart"]
    while {1} {
	set ch [$win get $i]

	if {[$win compare $i <= $lineStart]} {
	    set i $lineStart
	    break
	}

	if {[string is space $ch]} {
	    break
	}

	set i [$win index "$i - 1 chars"]
    }
    return $i
}

proc btext::clearHighlightClasses {win} {
    #no need to catch, because array unset doesn't complain
    #puts [array exists ::btext::highlight$win]

    btext::getAr $win highlight ar
    array unset ar

    btext::getAr $win highlightSpecialChars ar
    array unset ar

    btext::getAr $win highlightRegexp ar
    array unset ar

    btext::getAr $win highlightCharStart ar
    array unset ar

    btext::getAr $win classes ar
    array unset ar
}

#This is a proc designed to be overwritten by the user.
#It can be used to update a cursor or animation while
#the text is being highlighted.
proc btext::update {} {

}

proc btext::highlight {win start end {afterTriggered 0}} {
    btext::getAr $win config configAr

    if {$afterTriggered} {
	set configAr(highlightAfterId) ""
    }

    if {!$configAr(-highlight)} {
	return
    }

    set si $start
    set twin "$win._t"

    #The number of times the loop has run.
    set numTimesLooped 0
    set numUntilUpdate 600

    btext::getAr $win highlight highlightAr
    btext::getAr $win highlightSpecialChars highlightSpecialCharsAr
    btext::getAr $win highlightRegexp highlightRegexpAr
    btext::getAr $win highlightCharStart highlightCharStartAr

    while 1 {
	set res [$twin search -count length -regexp -- {([^\s\(\{\[\}\]\)\.\t\n\r;\"'\|,]+)} $si $end]
	if {$res == ""} {
	    break
	}

	set wordEnd [$twin index "$res + $length chars"]
	set word [$twin get $res $wordEnd]
	set firstOfWord [string index $word 0]

	if {[info exists highlightAr($word)] == 1} {
	    set wordAttributes [set highlightAr($word)]
	    foreach {tagClass color} $wordAttributes break

	    $twin tag add $tagClass $res $wordEnd
	    $twin tag configure $tagClass -foreground $color

	} elseif {[info exists highlightCharStartAr($firstOfWord)] == 1} {
	    set wordAttributes [set highlightCharStartAr($firstOfWord)]
	    foreach {tagClass color} $wordAttributes break

	    $twin tag add $tagClass $res $wordEnd
	    $twin tag configure $tagClass -foreground $color
	}
	set si $wordEnd

	incr numTimesLooped
	if {$numTimesLooped >= $numUntilUpdate} {
	    btext::update
	    set numTimesLooped 0
	}
    }

    foreach {ichar tagInfo} [array get highlightSpecialCharsAr] {
	set si $start
	foreach {tagClass color} $tagInfo break

	while 1 {
	    set res [$twin search -- $ichar $si $end]
	    if {"" == $res} {
		break
	    }
	    set wordEnd [$twin index "$res + 1 chars"]

	    $twin tag add $tagClass $res $wordEnd
	    $twin tag configure $tagClass -foreground $color
	    set si $wordEnd

	    incr numTimesLooped
	    if {$numTimesLooped >= $numUntilUpdate} {
		btext::update
		set numTimesLooped 0
	    }
	}
    }

    foreach {tagClass tagInfo} [array get highlightRegexpAr] {
	set si $start
	foreach {re color} $tagInfo break
	while 1 {
	    set res [$twin search -count length -regexp -- $re $si $end]
	    if {"" == $res} {
		break
	    }

	    set wordEnd [$twin index "$res + $length chars"]
	    $twin tag add $tagClass $res $wordEnd
	    $twin tag configure $tagClass -foreground $color
	    set si $wordEnd

	    incr numTimesLooped
	    if {$numTimesLooped >= $numUntilUpdate} {
		btext::update
		set numTimesLooped 0
	    }
	}
    }
}

proc btext::linemapToggleMark {win y} {
    btext::getAr $win config configAr

    if {!$configAr(-linemap_markable)} {
	return
    }

    set markChar [$win.l index @0,$y]
    set lineSelected [lindex [split $markChar .] 0]
    set line [$win.l get $lineSelected.0 $lineSelected.end]

    if {$line == ""} {
	return
    }

    btext::getAr $win linemap linemapAr

    if {[info exists linemapAr($line)] == 1} {
	#It's already marked, so unmark it.
	array unset linemapAr $line
	btext::linemapUpdate $win
	set type unmarked
    } else {
	#This means that the line isn't toggled, so toggle it.
	array set linemapAr [list $line {}]
	$win.l tag add lmark $markChar [$win.l index "$markChar lineend"]
	$win.l tag configure lmark -foreground $configAr(-linemap_select_fg) \
	    -background $configAr(-linemap_select_bg)
	set type marked
    }

    if {[string length $configAr(-linemap_mark_command)]} {
	uplevel #0 [linsert $configAr(-linemap_mark_command) end $win $type $line]
    }
}

#args is here because -yscrollcommand may call it
proc btext::linemapUpdate {win args} {
    if {[winfo exists $win.l] != 1} {
	return
    }

    set pixel 0
    set lastLine {}
    set lineList [list]
    set fontMetrics [font metrics [$win._t cget -font]]
    set incrBy [expr {1 + ([lindex $fontMetrics 5] / 2)}]

    while {$pixel < [winfo height $win.l]} {
	set idx [$win._t index @0,$pixel]

	if {$idx != $lastLine} {
	    set line [lindex [split $idx .] 0]
	    set lastLine $idx
	    lappend lineList $line
	}
	incr pixel $incrBy
    }

    btext::getAr $win linemap linemapAr

    $win.l delete 1.0 end
    set lastLine {}
    foreach line $lineList {
	if {$line == $lastLine} {
	    $win.l insert end "\n"
	} else {
	    if {[info exists linemapAr($line)]} {
		$win.l insert end "$line\n" lmark
	    } else {
		$win.l insert end "$line\n"
	    }
	}
	set lastLine $line
    }
    if {[llength $lineList] > 0} {
	linemapUpdateOffset $win $lineList
    }
    set endrow [lindex [split [$win._t index end-1c] .] 0]
    $win.l configure -width [string length $endrow]
}

# Starting with Tk 8.5 the text widget allows smooth scrolling; this
# code calculates the offset for the line numbering text widget and
# scrolls by the specified amount of pixels

if {![catch {
    package require Tk 8.5
}]} {
    proc btext::linemapUpdateOffset {win lineList} {
	# reset view for line numbering widget
	$win.l yview 0.0

	# find the first line that is visible and calculate the
	# corresponding line in the line numbers widget
	set lline 1
	foreach line $lineList {
	    set tystart [lindex [$win.t bbox $line.0] 1]
	    if {$tystart != ""} {
		break
	    }
	    incr lline
	}

	# return in case the line numbers text widget is not up to
	# date
	if {[catch {
	    set lystart [lindex [$win.l bbox $lline.0] 1]
	}]} {
	    return
	}

	# return in case the bbox for any of the lines returned an
	# empty value
	if {($tystart == "") || ($lystart == "")} {
	    return
	}

	# calculate the offset and then scroll by specified number of
	# pixels
	set offset [expr {$lystart - $tystart}]
	$win.l yview scroll $offset pixels
    }
}  else  {
    # Do not try to perform smooth scrolling if Tk is 8.4 or less.
    proc btext::linemapUpdateOffset {args} {}
}

proc btext::modified {win value} {
    btext::getAr $win config ar
    set ar(modified) $value
    event generate $win <<Modified>>
    return $value
}
