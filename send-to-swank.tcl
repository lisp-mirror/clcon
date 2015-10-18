## Copyright (c) 2015 Denis Budyak
## MIT License
## Formatting and sending messages to SWANK
## Reading from SWANK is in swank-io.tcl (e.g. ::tkcon::SwankReadMessageFromStream)

namespace eval ::sts {
}

# We have tcl object (string). Quote it so that it can be sent to swank as string
proc ::tkcon::QuoteTclStringForLisp {str} {
    regsub -all {[\ \\\"]} $str {\\&} s2
    # regsub -all {\n} $s2 {\n} s3
    set result [string cat \" $s2 \"]
    return $result
}

# Old misname for ::tkcon::QuoteTclStringForLisp
proc ::tkcon::QuoteLispObjToString {str} {
    return [QuoteTclStringForLisp $str]
}


# Given tcl list (of strings), quotes every strings and concatenates them space-separated.
proc ::tkcon::QuoteTclListOfStringsForLisp {list} {
    set result ""
    set delim ""
    foreach x $list {
        set result [string cat $delim $result [::tkcon::QuoteTclStringForLisp $x]]
        set delim " "
    }
    return $result
}


## from swank-protocol::emacs-rex
proc ::tkcon::CalculateThreadDesignatorForSwank {MsgFmtKind} {
    variable PRIV
    if {$MsgFmtKind == 0} {
        return $PRIV(SwankThread)
    } elseif {$MsgFmtKind == 1} {
        return ":repl-thread"
    } elseif {$MsgFmtKind == 2} {
        error "Thread designator must be supplied"
    } else { 
        error "Unknown MsgFmtKind"
    }
}

proc ::tkcon::FormatEmacsReturnMessageInner {value ThreadDesignator ContinuationCounter} {
    # (:emacs-return thread tag value)
    set msgNoLen "(:emacs-return $ThreadDesignator $ContinuationCounter $value)"
    EncodeAnySwankMessage $msgNoLen
}

proc ::tkcon::FormatSwankRexEvalMessageInner {cmd ThreadDesignator ContinuationCounter} {
    # commented out line is for my patched version which passes readtable
    # set msgNoLen "(:emacs-rex-rt $cmd \"COMMON-LISP-USER\" nil $ThreadDesignator $ContinuationCounter)"
    set msgNoLen "(:emacs-rex $cmd \"COMMON-LISP-USER\" $ThreadDesignator $ContinuationCounter)"
    EncodeAnySwankMessage $msgNoLen
}

proc ::tkcon::EncodeAnySwankMessage {msgNoLen} {
    set data [encoding convertto utf-8 $msgNoLen]
    #set LenMsg [string length $msgNoLen]
    set LenData [string length $data]
    #showVar LenMsg
    #showVar LenData
    set strLenHex [format "%06X" $LenData]
    set msgAndLen [string cat $strLenHex $data]
    return $msgAndLen
}

proc ::tkcon::FormatSwankRexEvalMessage {cmd MsgFmtKind {ThreadDesignator {}} {ContinuationCounter {}}} {
    if {$MsgFmtKind == 2} {
        return [EncodeAnySwankMessage $cmd]
    } elseif {$MsgFmtKind == 3} {
        return [FormatEmacsReturnMessageInner $cmd $ThreadDesignator $ContinuationCounter]
    }
    
    if { $ThreadDesignator eq {} } {
        set ThreadDesignator [CalculateThreadDesignatorForSwank $MsgFmtKind]
    }
    if { $ContinuationCounter eq {} } {
        set ContinuationCounter [GenContinuationCounter]
    }      

    return [FormatSwankRexEvalMessageInner $cmd $ThreadDesignator $ContinuationCounter]
}

proc ::tkcon::SwankMaybeWrapFormIntoListenerEval {form MsgFmtKind} {
    if {$MsgFmtKind == 1} {
        # QuoteLispObjToString ?
        set cmd [regsub -all {([\"\\])} $form {\\\0}]
        return "(swank-repl:listener-eval \"$cmd\")"
    } else {
        return $form
    }
}


## IDE orders EMACS to do some evaluation
proc ::tkcon::EvalInSwankAsync {form continuation {ThreadDesignator {}} {ContinuationCounter {}}} {
    ::tkcon::SendEventToSwank $form $continuation 0 $ThreadDesignator $ContinuationCounter
}

## SendEventToSwank . This function is responsible for all
# sending of events to SWANK.
# Args:
# Form - text of form to execute quoted as needed
# Continuation - body of proc to accept argument EventAsList
# MsgFmtKind
#   0 - normal eval (IDE orders EMACS to do evaluation)
#   1 - listener eval (form will be wrapped into (swank-repl:listener-eval ...)
#   2 - emacs-pong event (passed verbatim, ThreadDesignator and ContinuationCounter unneeded)
#   3 - emacs-return event (:emacs-return ContinuationCounter result) - form is a result, which is (:ok lisp-value), (:error lisp-kind . lisp-data), or (:abort). All lisp values must be quoted for passing by the caller of SendEventToSwank
#
#   ThreadDesignator - see swank::thread-for-evaluation
#   ContinuationCounter - required to identify addressee of swank's reply.
#        If not passed, it is calculated when needed
# 
proc ::tkcon::SendEventToSwank {form continuation {MsgFmtKind 1} {ThreadDesignator {}} {ContinuationCounter {}}} {
    variable PRIV

    set ConnectionName $::swcnn::CurrentSwankConnection

    if {$::swcnn::CurrentSwankConnection eq {}} {
        error "Attempt to SendEventToSwank with disconnected SWANK: $form"
    }
    
    upvar \#0 $ConnectionName con
    set sock $con(sock)

    # We don't need that for lisp. Some other translation should occur, hopefully we done it ok
    # Commend from old code:
    ## Sockets get \'s interpreted, so that users can
    ## send things like \n\r or explicit hex values
    #set cmd [subst -novariables -nocommands $form]

    ::CurIntPath "SendEventToSwank 2"

    if { $MsgFmtKind == 3 } {
        if {$ContinuationCounter eq {} || $ThreadDesignator eq {}} {
            error "ContinuationCounter (tag) and ThreadDesignator must be supplied for :emacs-return event"
        }
    }
    if { $ContinuationCounter eq {} && $MsgFmtKind != 2 } {
        set ContinuationCounter [GenContinuationCounter]
    }
   
    set cmd $form
    set cmd [SwankMaybeWrapFormIntoListenerEval $cmd $MsgFmtKind]
    set cmd [FormatSwankRexEvalMessage $cmd $MsgFmtKind $ThreadDesignator $ContinuationCounter]

    if { [lsearch -integer {0 1} $MsgFmtKind] >= 0  } {
      ::mprs::EnqueueContinuation $ContinuationCounter $continuation
    } else {
      if {$continuation ne {}} {
          error "Continuation must not be passed for this event kind"
      }
    }

    #putd "About to send to SWANK: $cmd"

    
    set code [catch {puts -nonewline $sock $cmd ; flush $sock} result]
    if {$code} {
        puts stderr "writing to socket returned code $code"
    }
    if {$code && [eof $sock]} {
        ## SWANK server stream died or disappeared
        puts stderr "writing to socket returned $code eof [eof $sock]"
        EvalSocketClosed $sock $ConnectionName
    }
    return -code $code $result
}


## ::tkcon::EmacsRex - from swank-protocol::emacs-rex
##  Docstring from emacs-rex:
##  (R)emote (E)xecute S-e(X)p.
##
##  Send an S-expression command to Swank to evaluate. The resulting response must
##  be read with read-response.
##  MsgFmtKind must be 1 if form is (swank-repl:listener-eval ...) or 0 otherwise
proc ::tkcon::SwankEmacsRex {form {MsgFmtKind 0}} {
    SendEventToSwank $form {} $MsgFmtKind
}

