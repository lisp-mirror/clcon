## Copyright (c) 1995-2011 Jeffrey Hobbs, jeff(a)hobbs(.)org
## source standard_disclaimer.tcl
## Copyright (c) Denis Budyak 2015

proc ::tkcon::BeginningOfLispSymbolRegexp {} {
     # ; return "\[^\\\\\]\[\[ \t\n\r(\",@\\'\]"
     return {[^\\][[:space:]$(),@'""]}
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
    showVarPutd type
    switch -glob $type {
        li* { set code [catch {ExpandLispSymbol $str} res] }
	pa* { set code [catch {ExpandPathname $str} res] }
	pr* { set code [catch {ExpandProcname $str} res] }
	v*  { set code [catch {ExpandVariable $str} res] }
	default {
	    # XXX could be extended to allow the results of all matches
	    # XXX to be amalgamted ... may be confusing to user
	    set match {}
	    foreach t $::tkcon::OPT(tclexpandorder) {
		set matchN [Expand$t $str 0]
                showVarPutd t
                showVarPutd matchN
                set match [concat $match $matchN]
	    }
            if {[llength $match] > 1} {
                regsub -all {([^\\]) } [ExpandBestMatch $match $str] {\1\\ } str
                set match [linsert $match 0 $str]
            } else {
                regsub -all {([^\\]) } $match {\1\\ } match
            }
            set res $match
	}
    }
    set len [llength $res]
    if {$len} {
	$w delete $tmp insert
        set xxx [lindex $res 0]
        showVarPutd res
        showVarPutd xxx
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
# Returns:	If findUniqueMatch then
#                 list containing longest unique match followed by all the
#                 possible further matches
#               else
#                 list of all possible further matches
##
proc ::tkcon::ExpandProcname {str {findUniqueMatch 1}} {

    # require at least a single character, otherwise continue
    if {$str eq ""} {return ""}

    set match1 [EvalAttached [list info commands $str*]]
    set ns [EvalAttached \
		"namespace children \[namespace current\] [list $str*]"]
    if {[llength $ns]==1} {
        set match2 [EvalAttached [list info commands ${ns}::*]]
    } else {
        set match2 $ns
    }
    showVarPutd match1
    showVarPutd match2
    set match [concat $match1 $match2]
    putd "Match in ExpandProcName"
    showVarPutd match
    if {$findUniqueMatch} {
        if {[llength $match] > 1} {
            regsub -all {([^\\]) } [ExpandBestMatch $match $str] {\1\\ } str
            set match [linsert $match 0 $str]
        } else {
            regsub -all {([^\\]) } $match {\1\\ } match
        }
    }
    return $match
}

## ::tkcon::ExpandMethodname - expand an NSF/XOTcl method name based on $str
# ARGS:	str	- partial proc name to expand
# Calls:	::tkcon::ExpandBestMatch
# Returns:	list containing longest unique match followed by all the
#		possible further matches
##
proc ::tkcon::ExpandMethodname {str {findUniqueMatch 1}} {

    # In a first step, obtain the typed-in cmd from the console
    set typedCmd [::tkcon::CmdGet $::tkcon::PRIV(console)]
    set obj [lindex $typedCmd 0]
    if {$obj eq $typedCmd} {
	# just a single word, can't be a method expansion
        return ""
    }
    # Get the full string after the object
    set sub [string trimleft [string range $typedCmd [string length [list $obj]] end]]
    if {[EvalAttached [list info exists ::nsf::version]]} {
	# Next Scripting Framework is loaded
	if {![EvalAttached [list ::nsf::object::exists $obj]]} {return ""}
	if {[string match ::* $sub]} {
	    # NSF allows dispatch of unregistered methods via absolute
	    # paths
	    set cmd "concat \[info commands $sub*\] \[namespace children \[namespace qualifiers $sub\] $sub*\]"
	} else {
	    set cmd [list $obj ::nsf::methods::object::info::lookupmethods -callprotection public -path -- $sub*]
	}
    } elseif {[EvalAttached [list info exists ::xotcl::version]]} {
	# XOTcl < 2.* is loaded
	if {![EvalAttached [list ::xotcl::Object isobject $obj]]} {return ""}
	set cmd [list $obj info methods $sub*]
    } else {
	# No NSF/XOTcl loaded
        return ""
    }

    set match [EvalAttached $cmd]
    if {$findUniqueMatch} {
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
    }
    return $match
}

## ::tkcon::ExpandVariable - expand a tcl variable name based on $str
# ARGS:	str	- partial tcl var name to expand
# Calls:	::tkcon::ExpandBestMatch
# Returns:	list containing longest unique match followed by all the
#		possible further matches
## 
proc ::tkcon::ExpandVariable {str {findUniqueMatch 1}} {

    # require at least a single character, otherwise continue
    if {$str eq ""} {return ""}

    if {[regexp {([^\(]*)\((.*)} $str junk ary str]} {
	## Looks like they're trying to expand an array.
	set match [EvalAttached [list array names $ary $str*]]
        if {$findUniqueMatch && ([llength $match] > 1)} {
            set vars $ary\([ExpandBestMatch $match $str]
            foreach var $match {lappend vars $ary\($var\)}
            return $vars
        } elseif {[llength $match] == 1} {
            set match $ary\($match\)
        } elseif {[llength $match] > 1} {
            set vars {}
            foreach var $match {lappend vars $ary\($var\)}
            return $vars
        }
	## Space transformation avoided for array names.
    } else {
	set match [EvalAttached [list info vars $str*]]
        if {$findUniqueMatch} {
            if {[llength $match] > 1} {
                regsub -all {([^\\]) } [ExpandBestMatch $match $str] {\1\\ } str
                set match [linsert $match 0 $str]
            } else {
                regsub -all {([^\\]) } $match {\1\\ } match
            }
        }
    }
    return $match
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
    showVarPutd e
    set ec [lindex $l 0]
    if {[llength $l]>1} {
	set eLength  [string length $e]; incr eLength -1
	set ei [string length $ec]; incr ei -1
	foreach l $l {
	    while {$ei>=$eLength && [string first $ec $l]} {
		set ec [string range $ec 0 [incr ei -1]]
	    }
	}
    }
    # This can happen if e is both a namespace name and proc name
    # In this case matches will be ::name and just name
    if {[string length $e]>[string length $ec]} {
        return $e
    } else {
        return $ec
    }
}


# Returns name at which insert stands
proc ::tkcon::GetTclNameAtInsert {w} {
    set exp_beg "\[^\\\\\]\[\[ \t\n\r\\\{\"$\]" 
    set i_b [$w search -backwards -regexp $exp_beg insert-1c 1.0]
    if {$i_b ne {}} {append i_b +2c} else {set i_b 1.0}
    set exp_end "\[^\\\\\]\[\\\[\\\] \t\n\r\\\{\}\"$\]"
    set i_e [$w search -forwards -regexp $exp_end insert end]
    showVarPutd i_e
    if {$i_e ne {}} {append i_e +1c} else {set i_e end-1c}
    set result [$w get $i_b $i_e]
    showVarPutd result
    return $result
}

#proc ::tkcon::TclListAllToplevelThings {} {
#    set procs [info procs *]
#    set vars [info vars *]
#    set result [concat $procs $vars]
#    return $result
#}

proc ::tkcon::TclListAllDefinedThingsInNamespace {namespace} {
    set procs [info procs ${namespace}::*]
    set vars [info vars ${namespace}::*]
    set children [namespace children ${namespace}]
    set result [concat $procs $vars $children]
    foreach child $children {
        set result [concat $result [TclListAllDefinedThingsInNamespace $child]]
    }
    return $result
}

proc ::tkcon::TclApropos {str} {
    # set e1 [TclListAllToplevelThings]
    # set AllNames [concat $e1 $e2]
    set AllNames [TclListAllDefinedThingsInNamespace "::"]
    set result [list]
    foreach x $AllNames {
        if {[string match -nocase *${str}* $x]} {
            lappend result $x
        }
    }
    return [lsort $result]
}

