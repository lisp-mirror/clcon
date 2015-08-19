## Copyright (c) 1995-2011 Jeffrey Hobbs, jeff(a)hobbs(.)org
## Copyright (c) 2015 Denis Budyak
## MIT License

# Main checkpoints are:
# ::tkcon::EvalInSwankSync - synchronously evaluate lisp in "t" swank thread and return result
# Dont forget to qualify all symbols you use in your command
# 

# General notes
# puts is aliased sometimes in tkcon so that it calls update idletasks.
# So, don't use idle in this file or switch to another debugging mechanism.

# Writing event handlers for lisp events:
# Event handler must call SheduleCheckSWANKEventQueue when appropriate to ensure
# that communication won't lock up. This protocol seem to be simplier to debug:
# if something is wrong, it just locks up.
# Beware that if event loop is called inside of your event handler,
# bad things can happen:
#
# 1. Connection can be broken
# 2. Window can disappear
# 3. Synchronous mode can switch on { $::mprs::SWANKIsInSyncMode == 1 }
# 4. What I forgot? 
# Check all situations and behave appropriately. 


# Initialize the ::mprs namespace (message parser)
# This is for SWANK communication-related stuff, though some parts are in ::tkcon namespace
namespace eval ::mprs {
}

proc ::tkcon::GenContinuationCounter {} {
    # See also swank-protocol::connection-request-counter
 variable ContinuationCounter
 if {![info exists ContinuationCounter]} {
    set ContinuationCounter 1
 } else {
    set ContinuationCounter [expr {$ContinuationCounter + 1}]
 }
 return $ContinuationCounter
}

## from swank-protocol::emacs-rex
proc ::tkcon::CalculateThreadDesignatorForSwank {ItIsListenerEval} {
    variable PRIV
    if {$ItIsListenerEval == 1} {
        return ":repl-thread"
    } else {
        return $PRIV(SwankThread)
    }
}

proc ::tkcon::FormatSwankRexEvalMessageInner {cmd ThreadDesignator ContinuationCounter} {
    # commented out line is for my patched version which passes readtable
    # set msgNoLen "(:emacs-rex-rt $cmd \"COMMON-LISP-USER\" nil $ThreadDesignator $ContinuationCounter)"
    set msgNoLen "(:emacs-rex $cmd \"COMMON-LISP-USER\" $ThreadDesignator $ContinuationCounter)"
    set strLenHex [format "%06X" [string length $msgNoLen]]
    set msgAndLen [string cat $strLenHex $msgNoLen]
    return $msgAndLen
}

proc ::tkcon::FormatSwankRexEvalMessage {cmd ItIsListenerEval {ThreadDesignator {}} {ContinuationCounter {}}} {
    if { $ThreadDesignator eq {} } {
        set ThreadDesignator [CalculateThreadDesignatorForSwank $ItIsListenerEval]
    }
    if { $ContinuationCounter eq {} } {
        set ContinuationCounter [GenContinuationCounter]
    }      

    return [FormatSwankRexEvalMessageInner $cmd $ThreadDesignator $ContinuationCounter]
}

proc ::tkcon::SwankMaybeWrapFormIntoListenerEval {form ItIsListenerEval} {
    if {$ItIsListenerEval == 1} {
        set cmd [regsub -all {([\"\\])} $form {\\\0}]
        return "(swank-repl:listener-eval \"$cmd\")"
    } else {
        return $form
    }
}


proc ::tkcon::myerror {text} {
    idebug on
    puts "myerror: $text" stderr
    tk_messageBox -title "myerror" -message $text
    idebug break
}

# e.g. .puts [::tkcon::EvalInSwankSync {(swank:simple-completions "sw" '"COMMON-LISP-USER")}]

# tries to evaluate code in sync mode and returns a result
# Can call inner event loop, but no async events are processed.
# If some async event handler is on the stack already and sheduled events,
# we can't help it. 
proc ::tkcon::EvalInSwankSync {lispcode} {
    #proc ::tkcon::FormatSwankRexEvalMessageInner {cmd ThreadDesignator ContinuationCounter} 
    variable SWANKIsInSyncMode
    variable SWANKSyncContinuation
    if { $SWANKIsInSyncMode == 1 } {
        myerror "::tkcon::EvalInSwankSync is not reenterable"
    }
    set SWANKIsInSyncMode 1
    try {
        return [::mprs::EvalInSwankSyncInner $lispcode]
    } finally {
        mprs::DeleteSyncEventsFromTheQueue
        set SWANKSyncContinuation {}
        set SWANKIsInSyncMode 0
        SheduleCheckSWANKEventQueue
    }
}

# See ::tkcon::EvalInSwankSync
proc ::mprs::EvalInSwankSyncInner {lispcode} {
    variable ::tkcon::SWANKSyncContinuation
    variable ::tkcon::SWANKEventQueue
    set ContinuationCounter [::tkcon::GenContinuationCounter]
    set ::tkcon::SWANKSyncContinuation $ContinuationCounter

    # send command
    ::tkcon::EvalInSwankAsync $lispcode 0 t $ContinuationCounter

    # Delay all async events from lisp. Process sync events. 
    while {1 == 1} {

        putd "EvalInSwankSyncInner goes vwait for SWANKEventQueue"

        vwait ::tkcon::SWANKEventQueue

        putd "EvalInSwankSyncInner woke up on vwait SWANKEventQueue"

        set processed [MaybeProcessSyncEventFromQueue]

        putd "MaybeProcessSyncEventFromQueue returned $processed"
        if { [lindex $processed 0] == 1} {
            set result [lindex $processed 1]
            putd "EvalInSwankSyncInner: about to return $result"
            return $result
        } else {
            # this was async event, it is now in the queue. Lets sleep further
        }
    }
}


proc ::mprs::DeleteSyncEventsFromTheQueue {} {
    while { 1 == 1  } {
        set a [ExtractSyncEventFromQueueIfExists]
        if { $a eq {} } {
            return
        }
        putd "Dropping sync event $a"
    }
}


## ItIsListenerEval must be 1 to wrap form into (swank-repl:listener-eval ...) or 0 otherwise
# If ContinuationCounter is not passed, it is calculated when needed
proc ::tkcon::EvalInSwankAsync {form {ItIsListenerEval 1} {ThreadDesignator {}} {ContinuationCounter {}}} {
    variable OPT
    variable PRIV

    #putd "entered EvalInSwank"

    # tcl escape: if lisp command starts from . , we (temporarily?) consider it as tcl escape
    if {[string index $form 0] eq "."} {
        ::tkcon::Main [string range $form 1 end]
        return
    }
    
    if {$PRIV(deadapp)} {
	if {![info exists PRIV(app)] || \
		[catch {eof $PRIV(app)} eof] || $eof} {
	    return
	} else {
	    set PRIV(appname) [string range $PRIV(appname) 5 end]
	    set PRIV(deadapp) 0
	    Prompt "\n\"$PRIV(app)\" alive\n" [CmdGet $PRIV(console)]
	}
    }

    # FIXME - we don't need that for lisp! Some other translation should occur!
    # Sockets get \'s interpreted, so that users can
    # send things like \n\r or explicit hex values
    #set cmd [subst -novariables -nocommands $form]

    set cmd $form
    
    putd "regsub result: $cmd"

    set cmd [SwankMaybeWrapFormIntoListenerEval $cmd $ItIsListenerEval]

    putd "wrapped to listener eval: $cmd"
    
    #putd [list $PRIV(app) $cmd]
    
    set cmd [FormatSwankRexEvalMessage $cmd $ItIsListenerEval $ThreadDesignator $ContinuationCounter]
    putd "About to send to SWANK: $cmd"

    # tr "About to send $cmd to SWANK"

    set code [catch {puts -nonewline $PRIV(app) $cmd ; flush $PRIV(app)} result]
        if {$code && [eof $PRIV(app)]} {
            ## Interpreter died or disappeared
            putd "$code eof [eof $PRIV(app)]"
            EvalSocketClosed $PRIV(app)
        }
    return -code $code $result
}


## ::tkcon::EmacsRex - from swank-protocol::emacs-rex
##  Docstring from emacs-rex:
##  (R)emote (E)xecute S-e(X)p.
##
##  Send an S-expression command to Swank to evaluate. The resulting response must
##  be read with read-response.
## ItIsListenerEval must be 1 if form is (swank-repl:listener-eval ...) or 0 otherwise
proc ::tkcon::SwankEmacsRex {form {ItIsListenerEval 0}} {
    EvalInSwankAsync $form $ItIsListenerEval
}

## swank-protocol::request-swank-require
proc ::tkcon::SwankRequestSwankRequire1 {requirement} {
    ::tkcon::SwankEmacsRex "(swank:swank-require '$requirement)"
}

## swank-protocol::request-init-presentations
proc ::tkcon::SwankRequestInitPresentations {} {
    myerror "presentations are disabled in EMACS setup"
    ::tkcon::SwankEmacsRex "(swank:init-presentations)"
}

proc ::tkcon::SwankNoteTclConnection {} {
    ::tkcon::SwankEmacsRex "(clco:note-this-is-tcl-connection)"
}

proc ::tkcon::SwankRequestCreateRepl {} {
    variable PRIV
    ::tkcon::SwankEmacsRex {(swank-repl:create-repl nil :coding-system "utf-8-unix")}
    set PRIV(SwankThread) 1
}

proc ::mprs::TypeTag {x} {
    string index $x 0
}

## ::mprs::Unleash 
# removes type tag of element so that it looks more like tcl data
# for nested structures, you also need to call Unleash for every element.
# See ::mprs::UnleashList
proc ::mprs::Unleash {x} {
    set tt [TypeTag $x]
    if { $tt eq "\{" } {
        string range $x 2 end-2
        #subst -nocommands -novariables [string range $x 2 end-2]
    } elseif { $tt eq ":" } {
        # problem can be with single keyword as space is added after it.
        if {[string index $x end] eq " "} {
            tr "We get space at the end of keyword $x"
        }
        string range $x 0 end
        #subst -nocommands -novariables [string range $x 0 end]
    } else {
        string range $x 1 end
        #subst -nocommands -novariables [string range $x 1 end]
    }
}

# list is tagged with l or {l . Returns its elements Unleashed
proc ::mprs::UnleashList {typedlist} {
    set TypedElements [Unleash $typedlist]
    lmap x $TypedElements {Unleash $x}
}


## minial testing facility
# tests are runned when code is loading (horrible!)
proc ::mprs::AssertEq {x y} {
    if {! ($x eq $y)} {
        ::tkcon::myerror "Assertion failure: $x eq $y"
    }
}

proc ::mprs::Consp {x} {
    string match {[l\\\{]} [TypeTag $x]
}

proc ::mprs::Car {x} {
    if {[Consp $x]} {
        lindex [Unleash $x] 0
    } else { ::tkcon::myerror "Car: $x is not a list"
    }
}

proc ::mprs::Cadr {x} {
    if {[Consp $x]} {
        lindex [Unleash $x] 1
    } else { ::tkcon::myerror "Car: $x is not a list"
    }
}

# test
::mprs::AssertEq [::mprs::Car [::mprs::Cadr {l:return {l:ok s(format\\ destination\\ control-string\\ &rest\\ format-arguments) } n160 }]] :ok


proc ::mprs::ExtractContinuationId {EventAsList} {
    set EventHead [lindex $EventAsList 0]
    if {[lsearch {:return :abort} $EventHead] >= 0} {
        return [Unleash [lindex $EventAsList 2]]               
    }
}

# must be called when SWANKIsInSyncMode and when SWANKSyncContinuation is set
proc ::mprs::ExtractSyncEventFromQueueIfExists {} {
    variable ::tkcon::SWANKIsInSyncMode
    variable ::tkcon::SWANKSyncContinuation 
    variable ::tkcon::SWANKEventQueue
    # control::control assert {$SWANKIsInSyncMode == 1}

    set i -1
    foreach Event $SWANKEventQueue {
        incr i

        putd "Checking if $Event is sync = $SWANKSyncContinuation"
        
        AssertEq [Consp $Event] 1
        putd "Passed AssertEq"
        set EventAsList [Unleash $Event]
        set ContinuationId [ExtractContinuationId $EventAsList]
        if { $ContinuationId == $SWANKSyncContinuation } {
            set SWANKEventQueue [lreplace $SWANKEventQueue $i $i]
            return $Event
        }
    }
    return {}
}

proc ::mprs::MaybeProcessSyncEventFromQueue {} {
    variable ::tkcon::SWANKIsInSyncMode
    # find if it is in SWANKSyncContinuation
    # if found, process it
    # note if abort is called, we throw so we need another read loop for sync
    # messages. I think we mast configure -blocking 1 and put async messages into a queue
    # also we need global var to distinguish if we are in sync mode.
    if {$SWANKIsInSyncMode == 0} {
        return
    }
    set Event [ExtractSyncEventFromQueueIfExists]
    if { $Event eq {} } {
        return {0}
    }

    set EventAsList [Unleash $Event]
    set EventHead [lindex $EventAsList 0]

    putd "EventHead = $EventHead"

    if {$EventHead eq ":abort"} {
        putd "Processing of :abort sync return is not done yet, throwing"
        throw "abort of sync eval"
    } elseif {$EventHead eq ":return"} {
        set Body [lindex $EventAsList 1]
        putd "Body = $Body"
        return [list 1 $Body]
    } else {
        putd "Unknown head $EventHead in sync reply $Event"
    }
    # assume event is processed (if it is not an :abort)
    return [list 1 $Event]
}

# this is an async event. Process it. E.g. call a continuation
proc ::mprs::ProcessAsyncEvent {EventAsList} {
    set ContinuationId [ExtractContinuationId $EventAsList]
    # we don't need to Unleash keywords
    set Head [Unleash [lindex $EventAsList 0]]
    if { $Head eq ":write-string" } {
        puts -nonewline [Unleash [lindex $EventAsList 1]]
        ::tkcon::SheduleCheckSWANKEventQueue
    } else {
        putd "ProcessAsyncEvent stub Id=$ContinuationId Data=[Unleash [lindex $EventAsList 1]]"
        ::tkcon::SheduleCheckSWANKEventQueue
    }
    return {}
} 

# If there is an event on the queue, process it.
proc ::mprs::ProcessFirstEventFromQueueAsyncrhonously {} {
    variable ::tkcon::SWANKEventQueue
    variable ::tkcon::SWANKIsInSyncMode
    
    # If we are in sync mode, we can't do that now. So we return.
    # User must ensure that on exit of async mode <<CheckSWANKEventQueue>> is generated
    if { $SWANKIsInSyncMode == 1 } {
        return 
    }

    # Pop event
    set Event [lindex $SWANKEventQueue 0]
    set SWANKEventQueue [lreplace $SWANKEventQueue 0 0]
    if { $Event eq {} } {
        return
    }
    
    # We only understand list-shaped events
    AssertEq [Consp $Event] 1
    set EventAsList [Unleash $Event]
    set EventHead [lindex $EventAsList 0]
    set ContinuationId [ExtractContinuationId $EventAsList]

    putd "ContinuationId = $ContinuationId"
    
    ProcessAsyncEvent $EventAsList 
}

## ::mprs::ProcessEventsFromQueueIfAppropriate
# Depending of SWANKIsInSyncMode state, processes either sync or async message
# This proc is called from sync event loop and from fileevent (async event processing sequence)
# In sync mode, tries to find sync message on the queue
# In async mode, takes first message from the queue
proc ::mprs::ProcessEventsFromQueueIfAppropriate {} {
    variable ::tkcon::SWANKEventQueue
    variable ::tkcon::SWANKIsInSyncMode
    # eof must be processed in Readable function itself    

    if { $SWANKIsInSyncMode == 1 } {
        set result MaybeProcessSyncEventFromQueue
        return $result
    } else {
        ProcessFirstEventFromQueueAsyncrhonously
        # return value makes no sence here
    }
}

## Generates <<CheckSWANKEventQueue>> event
# The event means that we must try to process some events from SWANK event queue
proc ::tkcon::SheduleCheckSWANKEventQueue {} {
    variable PRIV
    set w $PRIV(console)
    event generate $w <<CheckSWANKEventQueue>> -when tail
}


## Temporary procedure to handle input from SWANK
## does not have counterparts in swank-protocol!!
proc ::tkcon::TempSwankChannelReadable {sock} {
    variable SWANKEventQueue
    variable SWANKIsInSyncMode

    # First of all we must have checked disconnect event, but let's skip it for now
    set Event [SwankReadMessageString]

    # just for debugging 
    putd "message from socket: $Event"

    if { [string index $Event 0] eq "(" } {
        putd "Skipping lisp-formed event $Event"
    } else {
        putd "queue is $SWANKEventQueue . Lets post to it"
        lappend SWANKEventQueue $Event
        if { $SWANKIsInSyncMode == 0 } {
            SheduleCheckSWANKEventQueue
        } else {
            # no event sheduling is required as we are in vwait SWANKEventQueue
            # as we return from this handler, vwait will return and
            # sync processing will continue
        }
    }
}

## from swank-protocol::decode-integer
proc ::tkcon::SwankDecodeInteger {string} {
    set num [scan $string "%X"]
    return $num
}



## from swank-protocol::read-message-from-stream
proc ::tkcon::SwankReadMessageFromStream {stream} {
    set LengthString [read $stream 6]

    # eof checking by similarity with ::tkcon::EvalSocketEvent
    if {[eof $stream]} {
        EvalSocketClosed $stream
        return
    }

    set Length [SwankDecodeInteger $LengthString]
    set Buffer [read $stream $Length]

    if {[eof $stream]} {
        EvalSocketClosed $stream
        return
    }
    
    return $Buffer
}

## from swank-protocol::read-message-string
proc ::tkcon::SwankReadMessageString {} {
    variable PRIV
    set channel $PRIV(app)
    fconfigure $channel -blocking 1
    set result [SwankReadMessageFromStream $channel]
    # channel might have been closed while executing
    catch { fconfigure $channel -blocking 0 }
    return $result
}

# from swank-protocol::read-message. Partially unimplemented
proc ::tkcon::SwankReadMessage {} {
    # with-swank-syntax
    #  read-from-string
    return [SwankReadMessageString]
}


## Modelled after defmethod lime::connect :after
## some parts are not implemented yet
proc ::tkcon::SetupSwankConnection {channel console} {

    # this is tcl/tk specific
    bind $console <<CheckSWANKEventQueue>> ::mprs::ProcessEventsFromQueueIfAppropriate

    # this is not from lime!
    SwankNoteTclConnection 

  #(swank-protocol:request-connection-info connection)
  #;; Read the connection information message
  #(let* ((info (swank-protocol:read-message connection))
  #       (data (getf (getf info :return) :ok))
  #       (impl (getf data :lisp-implementation))
  #       (machine (getf data :machine)))
  #  (setf (connection-pid connection)
  #        (getf data :pid)
  
  #        (connection-implementation-name connection)
  #        (getf impl :name)

  #        (connection-implementation-version connection)
  #        (getf impl :version)

  #        (connection-machine-type connection)
  #        (getf machine :type)

  #        (connection-machine-version connection)
  #        (getf machine :version)

  #        (connection-swank-version connection)
  #        (getf data :version)))
    #;; Require some Swank modules
    #SwankRequestSwankRequire1 "swank-presentations"
    SwankRequestSwankRequire1 "swank-repl"

    # putd is for debugging here
    putd [SwankReadMessage]

    # Start it up
    # disabled at emacs SwankRequestInitPresentations

    SwankRequestCreateRepl

    # Wait for startup
    # putd is for debugging here
    putd [SwankReadMessage]

    #;; Read all the other messages, dumping them
    #(swank-protocol:read-all-messages connection))
    #
}


## ::tkcon::AttachSwank - called to setup SWANK connection
# ARGS:	name	- socket identifier 
# Results:	::tkcon::EvalAttached is recreated to send commands to socket
##
proc ::tkcon::AttachSwank {name} {
    variable PRIV
    variable OPT
    variable ATTACH
    variable $name
    upvar \#0 $name con
    set sock $con(sock)

    if {[llength [info level 0]] == 1} {
	# no args were specified, return the attach info instead
	return [AttachId]
    }
    set PRIV(displayWin) .
    global tcl_version
    if {[catch {eof $sock} res]} {
        return -code error "No known channel \"$sock\""
    } elseif {$res} {
        catch {close $sock}
        return -code error "Channel \"$sock\" returned EOF"
    }
    set app $sock
    set type swank

    if {![info exists app]} { set app $sock }

    # SwankThread is like swank-protocol::connection thread slot
    array set PRIV [list app $app appname $sock apptype $type deadapp 0 SwankThread t]

    ## ::tkcon::EvalAttached - evaluates the args in the attached interp
    ## args should be passed to this procedure as if they were being
    ## passed to the 'eval' procedure.  This procedure is dynamic to
    ## ensure evaluation occurs in the right interp.
    # ARGS:	args	- the command and args to evaluate
    ##
    set PRIV(namesp) ::
    set namespOK 0
    
    #interp alias {} ::tkcon::EvalAttached {} \
        #        ::tkcon::EvalSlave uplevel \#0

    interp alias {} ::tkcon::EvalAttached {} ::tkcon::EvalInSwankAsync {} \#0
    fconfigure $sock -buffering full -blocking 0
    
    # It is important we initialize connection before binding fileevent
    SetupSwankConnection $sock $PRIV(console)

    # The file event will just putd whatever data is found
    # into the interpreter
    fileevent $sock readable [list ::tkcon::TempSwankChannelReadable $sock]
    
    return [AttachId]
}

proc ::tkcon::OuterNewSwank {} {
    # ::tkcon::NewSwank 127.0.0.1 4009
    ::swcnn::MakeSwankConnection 127.0.0.1 4009
}

## ::tkcon::NewSwank - called to create a socket to connect to
# Results:	It will create a socket, and attach if requested
# FIXME logic does not follow logic from tkcon.
# Study relations between consoles and interpretes in tkon and fix thie sequence.
##
#proc ::tkcon::NewSwank {host port} {
#    variable PRIV
#    if {[catch {
#	set sock [socket $host $port]
#        fconfigure $sock -encoding utf-8
#    } err]} {
#	tk_messageBox -title "Socket Connection Error" \
#		-message "Unable to connect to \"$host:$port\":\n$err" \
#		-icon error -type ok
#    } else {
#	AttachSwank $sock
#    }
#}



## ::tkcon::ExpandLispSymbol (currently known as ExpandProcname)
# - expand a lisp symbol based on $str
# ARGS:	str	- partial proc name to expand
# Used to Call:	        ::tkcon::ExpandBestMatch
# Used to Return:	list containing longest unique match followed by all the
#		possible further matches
##
proc ::tkcon::ExpandLispSymbol str {

    
    # require at least a single character, otherwise continue
    if {$str eq ""} {return -code continue}
    
    # set LispCmd {(subseq (format nil "~{ ~A~}" (first (swank:simple-completions "a" "COMMON-LISP-USER"))) 1)}

    # string quoting is a bullshit here!
    putd "We must quote string $str better!"
    set LispCmd "(cl:progn (cl::sleep 0.5) (swank:simple-completions \"$str\" '\"COMMON-LISP-USER\"))"
   
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
    
    set match [::mprs::UnleashList [::mprs::Car $ExpansionsAndBestMatch]]
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

    
