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
# - expand a lisp symbol in the console based on $str
# ARGS:	w     - widget (passed to continuation)
#       str   - partial lisp symbol name to expand
#       tmp   - index in text widget (passed to continuation)
# Return:     - -code 12120374 to make caller return rather soon (see caller)
# Shedules:   ExpandLispSymbolC1 after returning from swank
##
# we hide ExpandLispSymbol for a while as we use expandsymbol as entrypoint to find-definition
## 
proc ::tkcon::ExpandLispSymbol {w str tmp} {
    variable OPT
    variable PRIV
    
    # require at least a single character, otherwise continue
    if {$str eq ""} {return -code continue}
    
    # string quoting is a bullshit here!
    set Quoted [QuoteLispObjToString $str]
    set PackageName [QuoteLispObjToString $PRIV(CurrentPackageDisplayName)]
    set LispCmd "(swank:completions $Quoted '$PackageName)"
    set OnReply [concat ::tkcon::ExpandLispSymbolC1 [list $w $str $tmp] "\$EventAsList"]]
    EvalInSwankAsync $LispCmd $OnReply :find-existing
    return -code 12120374 ""
}

#(:return
# (:ok
#  (("defcas" "defclass" "defconstant" "defconstant-uneql" "defconstant-uneql-name" "defconstant-uneql-new-value" "defconstant-uneql-old-value" "defgeneric" "defglobal" "defimplementation" "define-alien-routine" "define-alien-type" "define-alien-variable" "define-cas-expander" "define-compiler-macro" "define-condition" "define-hash-table-test" "define-load-time-global" "define-method-combination" "define-modify-macro" ...)
#   "def"))
    
proc ::tkcon::ExpandLispSymbolC1 {w str tmp EventAsList} {
    set ExpansionsAndBestMatch [::mprs::ParseReturnOk $EventAsList]

    set LeashedResult [lindex [::mprs::Unleash [lindex $EventAsList 1]] 1]

    if {[::mprs::Null $LeashedResult]} {
        bell
        return
    }
    
    set match [::mprs::UnleashListOfAtoms [lindex $ExpansionsAndBestMatch 0]]

    set match [ExpandMatchMagic $str $match]

    ::tkcon::ExpandC1 $w $str $tmp $match
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


global ::tkcon::PosStack [list]

proc ::tkcon::ReturnPos {} {
    variable ::tkcon::PosStack
    set last [lindex $PosStack end]
    if {$last != ""} {
        set ::tkcon::PosStack [lrange $::tkcon::PosStack 0 end-1]
        set pos [lindex $last 0]
        set filename [lindex $last 1]
        ::tkcon::EditFileAtOffset $filename $pos
    }
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


## ::tkcon::CallLispFunctionOnCurrentConsoleSymbol 
# Calls lisp_fn with symbol string and current package on current symbol text in concole text_widget, 
# and shedules tcl_continuation_fn {EventAsList} to be called upon return
proc ::tkcon::CallLispFunctionOnCurrentConsoleSymbol {text_widget lisp_fn tcl_continuation_fn} {
    variable PRIV

    set w $text_widget

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
    
    set Quoted [QuoteLispObjToString $str]
    set Package [QuoteLispObjToString $PRIV(CurrentPackageName)]
    set LispCmd "($lisp_fn $Quoted $Package)"
   
    ::tkcon::EvalInSwankAsync $LispCmd "[list $tcl_continuation_fn] \$EventAsList" {:repl-thread}
}


## ::tkcon::LispFindDefinition - 
# ARGS:	w	- console text widget in which to find a symbol
# See also: ::edt::FindSourceCommand ,  odu::find-source-command
## 
proc ::tkcon::LispFindDefinition {w} {
    CallLispFunctionOnCurrentConsoleSymbol $w "clcon-server:server-lookup-definition" "::tkcon::LispFindDefinitionInnerContinuation"
}

## ::tkcon::LispHyperdocLookup - 
# ARGS:	w	- console text widget in which to find a symbol
# See also: ::edt::FindSourceCommand ,  odu::find-source-command
## 
proc ::tkcon::LispHyperdocLookup {w} {
    CallLispFunctionOnCurrentConsoleSymbol $w "clcon-server:server-hyperdoc-lookup" "::ProcedureNop"
}




## ::tkcon::TclFindDefinition - clone of ::tkcon::Expand - 
# ARGS:	w	- text widget in which to expand str
# See also: ::edt::FindSourceCommand
## 
proc ::tkcon::TclFindDefinition {w} {
    variable ::tkcon::PosStack
    set str [GetTclNameAtInsert $w]
    if {$str eq {}} {
        tk_messageBox -parent $w -message "trying to edit empty definition"
    } else {
        catch { lappend ::tkcon::PosStack [list [$w index insert] [[$w cget -opened_file] cget -filename]] }
        ::record_definition::EditProcedure $str
    }
}

## This proc is evidently misplaced! FIXME
## Clone of ::edt::CurrentPathAndFileNameToConsole
proc ::tkcon::PasteAsLinuxFilename {w} {
    variable ::tkcon::PRIV
    set con $PRIV(console)
    set ClipText [clipboard get]
    set UnixFilename [::WindowsFileNameToUnix $ClipText]
    $con insert insert $UnixFilename
}
    
