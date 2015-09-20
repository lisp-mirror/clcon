## Copyright (c) 2015 Denis Budyak
## MIT License

# Applications of swank-io.tcl to tasks of our IDE

## ::tkcon::LispFindDefinitionInner
# Similar to ::tkcon::ExpandLispSymbol
proc ::tkcon::LispFindDefinitionInner str {
    variable PRIV

    # require at least a single character, otherwise continue
    if {$str eq ""} {return -code continue}
    
    # string quoting is a bullshit here!
    set Quoted [QuoteLispObjToString $str]
    set LispCmd "(cl:progn (clcon-server:server-lookup-definition $Quoted))"
   
    set SwankReply [::tkcon::EvalInSwankSync $LispCmd]
    
    putd "EvalInSwankSync returned $SwankReply"
    putd "car swankreply = [::mprs::Car $SwankReply]"
  
    if {[::mprs::Car $SwankReply] eq ":ok"} {
        # what about code injection? FIXME safety
        set TclCode "set w $PRIV(console); [::mprs::Unleash [::mprs::Cadr $SwankReply]]"
        putd "I will now eval code $TclCode"
        eval $TclCode
        $PRIV(console) see end
    } else {
        putd "ListDefinitions: I don't know what is [::mprs::Car $SwankReply]"
        return
    }
    return $str
}



# Similar to ::tkcon::ExpandLispSymbol
# FIXME - we need call compilation asynchronously, handle other results,
# print code context (from lisp)
# handle query to load failed compilation
proc ::tkcon::CompileLispFileTmp filename {
    variable PRIV

    set Quoted [QuoteLispObjToString $filename]
    set LispCmd "(clcon-server::compile-file-for-tcl $Quoted nil)"
   
    set SwankReply [::tkcon::EvalInSwankSync $LispCmd]
    
    putd "EvalInSwankSync returned $SwankReply"
    putd "car swankreply = [::mprs::Car $SwankReply]"
  
    if {[::mprs::Car $SwankReply] eq ":ok"} {
        # what about code injection? FIXME safety
        set TclCode "set w $PRIV(console); [::mprs::Unleash [::mprs::Cadr $SwankReply]]"
        putd "I will now eval code $TclCode"
        eval $TclCode
        $PRIV(console) see end
    } else {
        putd "ListDefinitions: I don't know what is [::mprs::Car $SwankReply]"
    }
}



## ::tkcon::ExpandLispSymbol (currently known as ExpandProcname)
# - expand a lisp symbol based on $str
# ARGS:	str	- partial proc name to expand
# Used to Call:	        ::tkcon::ExpandBestMatch
# Used to Return:	list containing longest unique match followed by all the
#		possible further matches
##
# we hid ExpandLispSymbol for a while as we use expandsymbol as entrypoint to find-definition
proc ::tkcon::ExpandLispSymbol str {
    variable OPT
    variable PRIV
    
    # require at least a single character, otherwise continue
    if {$str eq ""} {return -code continue}
    
    # set LispCmd {(subseq (format nil "~{ ~A~}" (first (swank:simple-completions "a" "COMMON-LISP-USER"))) 1)}

    # string quoting is a bullshit here!
    set Quoted [QuoteLispObjToString $str]
    if { $OPT(putd-enabled) == 1 } {
        set LispCmd "(cl:progn (cl::sleep 0.5) (swank:simple-completions $Quoted '\"COMMON-LISP-USER\"))"
    } else {
        set LispCmd "(swank:simple-completions $Quoted '\"COMMON-LISP-USER\")"
    }
   
    #testProc $LispCmd 1
    ##putd "Ok"
    ##tr [alias]
    set SwankReply [::tkcon::EvalInSwankSync $LispCmd]
    
    putd "EvalInSwankSync returned $SwankReply"
    putd "car swankreply = [::mprs::Car $SwankReply]"


#(:return
# (:ok
#  (("defcas" "defclass" "defconstant" "defconstant-uneql" "defconstant-uneql-name" "defconstant-uneql-new-value" "defconstant-uneql-old-value" "defgeneric" "defglobal" "defimplementation" "define-alien-routine" "define-alien-type" "define-alien-variable" "define-cas-expander" "define-compiler-macro" "define-condition" "define-hash-table-test" "define-load-time-global" "define-method-combination" "define-modify-macro" ...)
#   "def"))

    
    if {[::mprs::Car $SwankReply] eq ":ok"} {
        set ExpansionsAndBestMatch [::mprs::Cadr $SwankReply]
    } else {
        putd "ExpandLispSymbol: I don't know what is [::mprs::Car $SwankReply]"
        return
    }
    
    set match [::mprs::UnleashListOfAtoms [::mprs::Car $ExpansionsAndBestMatch]]
    # set MatchesList 

    putd "ExpandLispSymbol: match = $match"

    if {[llength $match] > 1} {
	regsub -all {([^\\]) } [ExpandBestMatch $match $str] {\1\\ } str
	set match [linsert $match 0 $str]
    } else {
	regsub -all {([^\\]) } $match {\1\\ } match
    }
    return -code [expr {$match eq "" ? "continue" : "break"}] $match
}


# Write a hyperlink to a text widget w
# problem is that we need mouse click to activate the hyperlink
proc ::tkcon::WriteActiveText {w text index code} {
    set tag [UniqueTag $w]
    $w tag configure $tag -foreground ForestGreen
    # $w insert output $text [list stdout $tag] \n stdout
    $w RoInsert $index $text $tag  
    $w tag bind $tag <Enter> [list $w tag configure $tag -under 1]
    $w tag bind $tag <Leave> [list $w tag configure $tag -under 0]
    $w tag bind $tag <ButtonRelease-1> $code
}


# Passive text
# Do we really need unique tag here? FIXME (add some more tags like stderr)
proc ::tkcon::WritePassiveText {w text index} {
    set tag [UniqueTag $w]
    $w tag configure $tag -foreground grey
    $w RoInsert $index $text\n [list stdout $tag]
}


proc ::tkcon::EditFileAtOffset {filename offset} {
    variable OPT
    $OPT(edit) -type file -offset $offset -- $filename
    #see offset - incoroporate it into edit.
    # $w mark set insert "0.0+ $offset chars"
    # focus -force $editor
}


## ::tkcon::LispFindDefinition - clone of ::tkcon::Expand - 
# ARGS:	w	- text widget in which to expand str
## 
proc ::tkcon::LispFindDefinition {w} {
    set exp [::tkcon::BeginningOfLispSymbolRegexp]
    set tmp [$w search -backwards -regexp $exp insert-1c limit-1c]
    if {[string compare {} $tmp]} {append tmp +2c} else {set tmp limit}
    set str [$w get $tmp insert]
    LispFindDefinitionInner $str
}

    

