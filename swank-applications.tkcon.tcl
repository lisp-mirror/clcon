## Copyright (c) 2015 Denis Budyak
## MIT License

# Applications of swank-io.tcl to tasks of our IDE

proc ::tkcon::LispFindDefinitionInnerContinuation {SwankReplyAsList} {
    variable PRIV
    set head [::mprs::Unleash [lindex $SwankReplyAsList 0]]

    if {$head eq ":return"} {
        set l [::mprs::Unleash [lindex $SwankReplyAsList 1]]
        set h2 [lindex $l 0]
        if {$h2 eq ":ok"} {
            # what about code injection? FIXME safety
            set TclCode "set w $PRIV(console); [::mprs::Unleash [lindex $l 1]]"
            putd "I will now eval code $TclCode"
            eval $TclCode
            $PRIV(console) see end
        } else {
            error "ListDefinitions: I don't know what is $h2"
        }
    } else {
        error "ListDefinitions: unknown her $h"
    }
    return
}

## ::tkcon::ExpandLispSymbol
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
        set LispCmd "(cl:progn (swank:simple-completions $Quoted '\"COMMON-LISP-USER\"))"
    } else {
        set LispCmd "(swank:simple-completions $Quoted '\"COMMON-LISP-USER\")"
    }
   
    #testProc $LispCmd 1
    ##putd "Ok"
    ##tr [alias]
    set SwankReply [::tkcon::EvalInSwankSync $LispCmd]
    
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

    if {[::mprs::Null [::mprs::Car $ExpansionsAndBestMatch]]} {
        return -code break ""
    }
    
    set match [::mprs::UnleashListOfAtoms [::mprs::Car $ExpansionsAndBestMatch]]
    # set MatchesList 

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
    ::ro_out::I $w $index $text $tag  
    $w tag bind $tag <Enter> [list $w tag configure $tag -under 1]
    $w tag bind $tag <Leave> [list $w tag configure $tag -under 0]
    $w tag bind $tag <ButtonRelease-1> $code
}


# Passive text
# Do we really need unique tag here? FIXME (add some more tags like stderr)
proc ::tkcon::WritePassiveText {w text index} {
    set tag [UniqueTag $w]
    $w tag configure $tag -foreground grey
    ::ro_out::I $w $index $text\n [list stdout $tag]
}


# Opens file at that offset (in chars). See also ::tkcon::EditFileAtLine
proc ::tkcon::EditFileAtOffset {filename offset} {
    variable OPT
    $OPT(edit) -type file -wrap char -offset $offset -- $filename
    #see offset - incoroporate it into edit.
    # $w mark set insert "1.0+ $offset chars"
    # focus -force $editor
}

proc ::tkcon::EditFileAtLine {filename line} {
    EditFileAtOffset $filename [string cat $line ".0"]
}

## ::tkcon::LispFindDefinition - 
# ARGS:	w	- console text widget in which to find a symbol
# See also: ::edt::FindSourceCommand
## 
proc ::tkcon::LispFindDefinition {w} {
    variable PRIV
    set exp [::tkcon::BeginningOfLispSymbolRegexp]

    if {[$w compare insert >= limit]} {
        set SearchStartPos {limit -1c}
    } else {
        set SearchStartPos {insert linestart -1c}
    }
    
    set tmp [$w search -backwards -regexp $exp insert-1c $SearchStartPos]
    if {[string compare {} $tmp]} {append tmp +2c} else {set tmp "$SearchStartPos +1c"}
    set tmp2 [$w search -regexp $exp $tmp]
    if {[string compare {} $tmp2]} {append tmp2 +1c} else {set tmp2 {insert lineend}}
    set str [$w get $tmp $tmp2]

    if {$str eq ""} {
        set con $PRIV(console)
        WritePassiveText $con "No symbol at cursor" output
        $con see insert
        return
    }
    
    # string quoting is a bullshit here!
    set Quoted [QuoteLispObjToString $str]
    set LispCmd "(cl:progn (clcon-server:server-lookup-definition $Quoted))"
   
    ::tkcon::EvalInSwankAsync $LispCmd {::tkcon::LispFindDefinitionInnerContinuation $EventAsList} {:find-existing}
    
}



## ::tkcon::TclFindDefinition - clone of ::tkcon::Expand - 
# ARGS:	w	- text widget in which to expand str
# See also: ::edt::FindSourceCommand
## 
proc ::tkcon::TclFindDefinition {w} {
    set str [GetTclNameAtInsert $w]
    if {$str eq {}} {
        tk_messageBox -parent $w -message "trying to edit empty definition"
    } else {
        ::record_definition::EditProcedure $str
    }
}



