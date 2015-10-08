## Copyright (c) 1995-2011 Jeffrey Hobbs, jeff(a)hobbs(.)org
## source standard_disclaimer.tcl
## Copyright (c) Denis Budyak 2015

proc ::tkcon::BeginningOfLispSymbolRegexp {} {
    return "\[^\\\\\]\[\[ \t\n\r\\\(\",@\\'\]"
}

## ::tkcon::Expand - 
# ARGS:	w	- text widget in which to expand str
# 	type	- type of expansion (path / proc / variable)
# Calls:	::tkcon::Expand(Pathname|Procname|Variable)
# Outputs:	The string to match is expanded to the longest possible match.
#		If ::tkcon::OPT(showmultiple) is non-zero and the user longest
#		match equaled the string to expand, then all possible matches
#		are output to stdout.  Triggers bell if no matches are found.
# Returns:	number of matches found
## 
proc ::tkcon::Expand {w {type ""}} {
    switch -glob $type {
        li* {
            # we might also want to pass current line to lisp 
            set exp [::tkcon::BeginningOfLispSymbolRegexp]
        }
        default { set exp "\[^\\\\\]\[\[ \t\n\r\\\{\"$\]" }
    }
    set tmp [$w search -backwards -regexp $exp insert-1c limit-1c]
    if {[string compare {} $tmp]} {append tmp +2c} else {set tmp limit}
    set str [$w get $tmp insert]
    # Expand procs can return "break" to indicate not to try further
    # matches, otherwise "continue" says "I got nothing, continue on"
    # We can ignore return codes from the specific expand type checks
    switch -glob $type {
        li* { set code [catch {ExpandLispSymbol $str} res] }
	pa* { set code [catch {ExpandPathname $str} res] }
	pr* { set code [catch {ExpandProcname $str} res] }
	v*  { set code [catch {ExpandVariable $str} res] }
	default {
	    # XXX could be extended to allow the results of all matches
	    # XXX to be amalgamted ... may be confusing to user
	    set res {}
	    foreach t $::tkcon::OPT(expandorder) {
		set code [catch {Expand$t $str} res]
		if {$code == 0 || $code == 3} { break }
		set res {}
	    }
	}
    }
    set len [llength $res]
    if {$len} {
	$w delete $tmp insert
	$w insert $tmp [lindex $res 0]
	if {$len > 1} {
	    if {$::tkcon::OPT(showmultiple) && \
		    ![string compare [lindex $res 0] $str]} {
		puts stdout [lsort [lreplace $res 0 0]]
	    }
	}
    } else { bell }
    return [incr len -1]
}


## ::tkcon::ExpandPathname - expand a file pathname based on $str
## This is based on UNIX file name conventions
# ARGS:	str	- partial file pathname to expand
# Calls:	::tkcon::ExpandBestMatch
# Returns:	list containing longest unique match followed by all the
#		possible further matches
## 
proc ::tkcon::ExpandPathname str {

    # require at least a single character, otherwise continue
    if {$str eq ""} {return -code continue}

    set pwd [EvalAttached pwd]
    # Cause a string like {C:/Program\ Files/} to become "C:/Program Files/"
    regsub -all {\\([][ ])} $str {\1} str
    if {[catch {EvalAttached [list cd [file dirname $str]]} err]} {
	return -code error $err
    }
    set dir [file tail $str]
    ## Check to see if it was known to be a directory and keep the trailing
    ## slash if so (file tail cuts it off)
    if {[string match */ $str]} { append dir / }
    # Create a safely glob-able name
    regsub -all {([][])} $dir {\\\1} safedir
    if {[catch {lsort [EvalAttached [list glob $safedir*]]} m]} {
	set match {}
    } else {
	if {[llength $m] > 1} {
	    global tcl_platform
	    if {[string match windows $tcl_platform(platform)]} {
		## Windows is screwy because it's case insensitive
		set tmp [ExpandBestMatch [string tolower $m] \
			[string tolower $dir]]
		## Don't change case if we haven't changed the word
		if {[string length $dir]==[string length $tmp]} {
		    set tmp $dir
		}
	    } else {
		set tmp [ExpandBestMatch $m $dir]
	    }
	    if {[string match */* $str]} {
		set tmp [string trimright [file dirname $str] /]/$tmp
	    }
	    regsub -all {([^\\])([][ ])} $tmp {\1\\\2} tmp
	    set match [linsert $m 0 $tmp]
	} else {
	    ## This may look goofy, but it handles spaces in path names
	    eval append match $m
	    if {[file isdirectory $match]} {append match /}
	    if {[string match */* $str]} {
		set match [string trimright [file dirname $str] /]/$match
	    }
	    regsub -all {([^\\])([][ ])} $match {\1\\\2} match
	    ## Why is this one needed and the ones below aren't!!
	    set match [list $match]
	}
    }
    EvalAttached [list cd $pwd]
    return -code [expr {$match eq "" ? "continue" : "break"}] $match
}

## ::tkcon::ExpandProcname - expand a tcl proc name based on $str
# ARGS:	str	- partial proc name to expand
# Calls:	::tkcon::ExpandBestMatch
# Returns:	list containing longest unique match followed by all the
#		possible further matches
##
proc ::tkcon::ExpandProcname str {

    # require at least a single character, otherwise continue
    if {$str eq ""} {return -code continue}

    set match [EvalAttached [list info commands $str*]]
    if {[llength $match] == 0} {
	set ns [EvalAttached \
		"namespace children \[namespace current\] [list $str*]"]
	if {[llength $ns]==1} {
	    set match [EvalAttached [list info commands ${ns}::*]]
	} else {
	    set match $ns
	}
    }
    if {[llength $match] > 1} {
	regsub -all {([^\\]) } [ExpandBestMatch $match $str] {\1\\ } str
	set match [linsert $match 0 $str]
    } else {
	regsub -all {([^\\]) } $match {\1\\ } match
    }
    return -code [expr {$match eq "" ? "continue" : "break"}] $match
}

## ::tkcon::ExpandMethodname - expand an NSF/XOTcl method name based on $str
# ARGS:	str	- partial proc name to expand
# Calls:	::tkcon::ExpandBestMatch
# Returns:	list containing longest unique match followed by all the
#		possible further matches
##
proc ::tkcon::ExpandMethodname str {

    # In a first step, obtain the typed-in cmd from the console
    set typedCmd [::tkcon::CmdGet $::tkcon::PRIV(console)]
    set obj [lindex $typedCmd 0]
    if {$obj eq $typedCmd} {
	# just a single word, can't be a method expansion
        return -code continue
    }
    # Get the full string after the object
    set sub [string trimleft [string range $typedCmd [string length [list $obj]] end]]
    if {[EvalAttached [list info exists ::nsf::version]]} {
	# Next Scripting Framework is loaded
	if {![EvalAttached [list ::nsf::object::exists $obj]]} {return -code continue}
	if {[string match ::* $sub]} {
	    # NSF allows dispatch of unregistered methods via absolute
	    # paths
	    set cmd "concat \[info commands $sub*\] \[namespace children \[namespace qualifiers $sub\] $sub*\]"
	} else {
	    set cmd [list $obj ::nsf::methods::object::info::lookupmethods -callprotection public -path -- $sub*]
	}
    } elseif {[EvalAttached [list info exists ::xotcl::version]]} {
	# XOTcl < 2.* is loaded
	if {![EvalAttached [list ::xotcl::Object isobject $obj]]} {return -code continue}
	set cmd [list $obj info methods $sub*]
    } else {
	# No NSF/XOTcl loaded
        return -code continue
    }

    set match [EvalAttached $cmd]
    if {[llength $match] > 1} {
	regsub -all {([^\\]) } [ExpandBestMatch $match $str] {\1\\ } bestMatch
	if {$str eq "" && [string match "* " $bestMatch]} {
	    set match [linsert $match 0 ""]
	} else {
	    regsub -all {\\ } $bestMatch { } bestMatch
	    set match [linsert $match 0 [lindex $bestMatch end]]
	}
    } else {
	set match [lindex [lindex $match 0] end]
    }
    return -code break $match
}

## ::tkcon::ExpandVariable - expand a tcl variable name based on $str
# ARGS:	str	- partial tcl var name to expand
# Calls:	::tkcon::ExpandBestMatch
# Returns:	list containing longest unique match followed by all the
#		possible further matches
## 
proc ::tkcon::ExpandVariable str {

    # require at least a single character, otherwise continue
    if {$str eq ""} {return -code continue}

    if {[regexp {([^\(]*)\((.*)} $str junk ary str]} {
	## Looks like they're trying to expand an array.
	set match [EvalAttached [list array names $ary $str*]]
	if {[llength $match] > 1} {
	    set vars $ary\([ExpandBestMatch $match $str]
	    foreach var $match {lappend vars $ary\($var\)}
	    return $vars
	} elseif {[llength $match] == 1} {
	    set match $ary\($match\)
	}
	## Space transformation avoided for array names.
    } else {
	set match [EvalAttached [list info vars $str*]]
	if {[llength $match] > 1} {
	    regsub -all {([^\\]) } [ExpandBestMatch $match $str] {\1\\ } str
	    set match [linsert $match 0 $str]
	} else {
	    regsub -all {([^\\]) } $match {\1\\ } match
	}
    }
    return -code [expr {$match eq "" ? "continue" : "break"}] $match
}

## ::tkcon::ExpandBestMatch2 - finds the best unique match in a list of names
## Improves upon the speed of the below proc only when $l is small
## or $e is {}.  $e is extra for compatibility with proc below.
# ARGS:	l	- list to find best unique match in
# Returns:	longest unique match in the list
## 
proc ::tkcon::ExpandBestMatch2 {l {e {}}} {
    set s [lindex $l 0]
    if {[llength $l]>1} {
	set i [expr {[string length $s]-1}]
	foreach l $l {
	    while {$i>=0 && [string first $s $l]} {
		set s [string range $s 0 [incr i -1]]
	    }
	}
    }
    return $s
}

## ::tkcon::ExpandBestMatch - finds the best unique match in a list of names
## The extra $e in this argument allows us to limit the innermost loop a
## little further.  This improves speed as $l becomes large or $e becomes long.
# ARGS:	l	- list to find best unique match in
# 	e	- currently best known unique match
# Returns:	longest unique match in the list
## 
proc ::tkcon::ExpandBestMatch {l {e {}}} {
    set ec [lindex $l 0]
    if {[llength $l]>1} {
	set e  [string length $e]; incr e -1
	set ei [string length $ec]; incr ei -1
	foreach l $l {
	    while {$ei>=$e && [string first $ec $l]} {
		set ec [string range $ec 0 [incr ei -1]]
	    }
	}
    }
    return $ec
}
