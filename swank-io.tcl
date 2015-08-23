## Copyright (c) 1995-2011 Jeffrey Hobbs, jeff(a)hobbs(.)org
## Copyright (c) 2015 Denis Budyak
## MIT License

# Main checkpoints are:
# ::tkcon::EvalInSwankAsync {form continuation {ItIsListenerEval 1} {ThreadDesignator {}} {ContinuationCounter {}}  - normal evaluation
# ::tkcon::EvalInSwankSync - synchronously evaluate lisp in "t" swank thread and return result
# ::tkcon::EvalInSwankFromConsole - especially for evaluation of command typed in from the console
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
    variable ContinuationsDict {}
}

# Load conses 
TkconSourceHere cons.tcl

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
    ::tkcon::EvalInSwankAsync $lispcode {} 0 t $ContinuationCounter

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
        puts stderr "Dropping sync event $a. This is likely a FIXME in swank-io.tcl. Continuation will persist"
    }
}


## ItIsListenerEval must be 1 to wrap form into (swank-repl:listener-eval ...) or 0 otherwise
# If ContinuationCounter is not passed, it is calculated when needed
proc ::tkcon::EvalInSwankAsync {form continuation {ItIsListenerEval 1} {ThreadDesignator {}} {ContinuationCounter {}}} {
    variable PRIV

    set ConnectionName $PRIV(SwankConnection)
    upvar \#0 $ConnectionName con
    set sock $con(sock)
    putd "I think socket stream is $sock"

    putd "Here we must check if socket is dead, but this is skipped"
#   if {$PRIV(deadapp)} {
#	if {![info exists PRIV(app)] || \
#		[catch {eof $PRIV(app)} eof] || $eof} {
#	    return
#	} else {
#	    set PRIV(appname) [string range $PRIV(appname) 5 end]
#	    set PRIV(deadapp) 0
#	    Prompt "\n\"$PRIV(app)\" alive\n" [CmdGet $PRIV(console)]
#	}
#   }

    # We don't need that for lisp. Some other translation should occur, hopefully we done it ok
    # Commend from old code:
    ## Sockets get \'s interpreted, so that users can
    ## send things like \n\r or explicit hex values
    #set cmd [subst -novariables -nocommands $form]

    ::CurIntPath "EvalInSwankAsync 2"
    
    if { $ContinuationCounter eq {} } {
        set ContinuationCounter [GenContinuationCounter]
    }
   
    set cmd $form
    
    putd "regsub result: $cmd"

    set cmd [SwankMaybeWrapFormIntoListenerEval $cmd $ItIsListenerEval]

    putd "wrapped to listener eval: $cmd"
    
    set cmd [FormatSwankRexEvalMessage $cmd $ItIsListenerEval $ThreadDesignator $ContinuationCounter]

    ::mprs::EnqueueContinuation $ContinuationCounter $continuation 

    putd "About to send to SWANK: $cmd"

    
    set code [catch {puts -nonewline $sock $cmd ; flush $sock} result]
        if {$code && [eof $sock]} {
            ## SWANK server stream died or disappeared
            putd "$code eof [eof $sock]"
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
## ItIsListenerEval must be 1 if form is (swank-repl:listener-eval ...) or 0 otherwise
proc ::tkcon::SwankEmacsRex {form {ItIsListenerEval 0}} {
    EvalInSwankAsync $form {} $ItIsListenerEval
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
    } elseif { [ContinuationExistsP $ContinuationId ] == 1 } {
        # we should have generated event which would evaluate continuation later.
        # but what we will do with sync events then?
        # we must either run all continations asynchronously, either run all continuations synchronously.
        putd "About to RunContinuation for $ContinuationId"
        RunContinuation $ContinuationId $EventAsList
    } else {
        putd "ProcessAsyncEvent - skipping Id=$ContinuationId Data=[Unleash [lindex $EventAsList 1]]"
        ::tkcon::SheduleCheckSWANKEventQueue
    }
    return {}
} 


## Continuations work for sync or async event
# Code accepts event in $Event variable which contains event unleashed one time
# code might look like this
# "::tkcon::EvalInSwankFromConsoleContinuation $w \$Event $cmd"
# where w and cmd are local variables
proc ::mprs::EnqueueContinuation {ContinuationId code} {
    variable ContinuationsDict
    dict set ContinuationsDict $ContinuationId [list {Event} $code]
}


proc ::mprs::ContinuationExistsP {ContinuationId} {
    variable ContinuationsDict
    if { [dict exists $ContinuationsDict $ContinuationId] } {
        return 1
    } else {
        return 0
    }
}

## We know continuiation exists. Runs its continuation synchronously. 
proc ::mprs::RunContinuation {ContinuationId EventAsList} {
    variable ContinuationsDict
    set Continuation [dict get $ContinuationsDict $ContinuationId]
    dict unset ContinuationsDict $ContinuationId
    apply $Continuation $EventAsList
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
    upvar \#0 $PRIV(SwankConnection) con
    set channel $con(sock)
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

proc ::tkcon::DisconnectFromSwank {} {
    variable PRIV
    set name $PRIV(SwankConnection)
    set PRIV(SwankConnection) {}
    ::swcnn::TerminateConnection $name
    Prompt
}             


## ::tkcon::AttachSwank - called to setup SWANK connection
# ARGS:	name	- swank connection name  
# Results:	::tkcon::EvalAttached is recreated to send commands to socket
##
proc ::tkcon::AttachSwank {name} {
    variable PRIV
    variable OPT
    variable ATTACH
    variable $name
    upvar \#0 $name con
    set sock $con(sock)
    puts stderr "WARNING! tkcon allows for several consoles, but do not try to have more than one SWANK attachment simultanously" 

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


    set PRIV(SwankThread) t
    set PRIV(SwankConnection) $name

    # interp alias {} ::tkcon::EvalAttached {} ::tkcon::EvalInSwankAsync {} \#0

    if { $con(state) eq "socket_only" } {
        fconfigure $sock -buffering full -blocking 0
    
        # It is important we initialize connection before binding fileevent
        SetupSwankConnection $sock $PRIV(console)

        # The file event will just putd whatever data is found
        # into the interpreter
        fileevent $sock readable [list ::tkcon::TempSwankChannelReadable $sock]

        set con(state) "initialized"
    } elseif { $con(state) eq "initialized" } {
        # do nothing
        putd "Initialized already"
        # We can't reach this point as we call AttachSwank only from OuterNewSwank
    } else {
        myerror "Unexpected state $con(state)"
    }

    Prompt
    
    return [AttachId]
}

proc ::tkcon::OuterNewSwank {} {
    # ::tkcon::NewSwank 127.0.0.1 4009
    variable OPT
    ::swcnn::MakeSwankConnection $OPT(swank-ip) $OPT(swank-port)
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


## ::tkcon::LispFindDefinitionInner
# Similar to ::tkcon::ExpandLispSymbol
proc ::tkcon::LispFindDefinitionInner str {
    variable PRIV

    # require at least a single character, otherwise continue
    if {$str eq ""} {return -code continue}
    
    # string quoting is a bullshit here!
    putd "We must quote string $str better!"
    set LispCmd "(cl:progn (clcon-server:server-lookup-definition \"$str\"))"
   
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
    putd "We must quote string $str better!"
    if { $OPT(putd-enabled) == 1 } {
        set LispCmd "(cl:progn (cl::sleep 0.5) (swank:simple-completions \"$str\" '\"COMMON-LISP-USER\"))"
    } else {
        set LispCmd "(swank:simple-completions \"$str\" '\"COMMON-LISP-USER\")"
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

proc ::tkcon::TclEscapeP { cmd } {
    if {[string index $cmd 0] eq "."} {
        return 1
    } else {
        return 0
    }
}

# Shows prompt, records history
proc ::tkcon::DoAfterCommand {} {
    variable PRIV
    Prompt
    set PRIV(event) [EvalSlave history nextid]
}

proc ::tkcon::EvalTclEscape { w form } {
    # tcl escape: if lisp command starts from . , we (temporarily?) consider it as tcl escape
    # ... - tkcon main
    # .. - just eval in slave interpreter
    # . - add ::clconcmd:: to resolve clcon command

    if {[string range $form 0 2] eq "..."} {
        set form [string cat "tkcon main {" [string range $form 3 end] "}"]
    } elseif {[string range $form 0 1] eq ".."} {
        set form [string range $form 2 end]
    } else {
        set form [string cat "::clconcmd::" [string range $form 1 end]]
    }
    # I believe this code is called in main interpreter
    # It seems that PRIV and OPT are only available in main interpreter
    # puts "EvalAttached = [interp alias ::tkcon::EvalAttached]"
    
    # It looks like error management is done by EvalAttached so we
    # don't need it
    
    set res [::tkcon::EvalAttached $form]
    
    # this is lame but it works!
    # Correct code for working with results see in EvalAttached or something like this.
    puts $res 

    return 
}


## Clone of ::tkcon::Eval (see clcon.tcl) and ::tkcon::EvalCmd
# Some code is lost while copying... 
proc ::tkcon::EvalInSwankFromConsole { w } {
    variable OPT
    variable PRIV
    
    set cmd [CmdGet $w]
    # we do not split commands
    $w mark set insert end-1c
    $w insert end \n

    #Code from EvalCmd follows

    $w mark set output end
    if {$cmd ne ""} {
	set code 0
        
	if {$OPT(subhistory)} {
            # do nothing. In good time, take the code from EvalCmd
	}
	if {$code} {
            # this should never happen in clcon, as we removed previous block
	    $w insert output $cmd\n stderr
	} else {
	    ## We are about to evaluate the command, so move the limit
	    ## mark to ensure that further <Return>s don't cause double
	    ## evaluation of this command - for cases like the command
	    ## has a vwait or something in it
	    $w mark set limit end

            set IsTcl [TclEscapeP $cmd]
            if { $IsTcl } {
                set code [catch {EvalTclEscape $w $cmd} res]
            } else {
                set code [catch {
                    # FIXME. I suppose this is slow. How to use apply here? Budden
                    EvalInSwankAsync $cmd \
                        "::tkcon::EvalInSwankFromConsoleContinuation $w \$Event $cmd"
                } res]
            }

            # for tcl escape, subsequent lines are executed after a command
            # for lisp, some of subsequent lines are executed after SENDING a command
            # see EvalInSwankFromConsoleContinuation
               
            if {$code == 1} {
                set PRIV(errorInfo) $::errorInfo
                # was - "Socket-based errorInfo not available", but command can fail on tcl side
            }
	    if {![winfo exists $w]} {
		# early abort - must be a deleted tab
		return
	    }

            
            if { $IsTcl } {
                # for lisp, we add to history when command returns
                AddSlaveHistory $cmd
            }

            if {$code} {
		if {$OPT(hoterrors)} {
		    set tag [UniqueTag $w]
		    $w insert output $res [list stderr $tag] \n stdout
		    $w tag bind $tag <Enter> \
			    [list $w tag configure $tag -under 1]
		    $w tag bind $tag <Leave> \
                        [list $w tag configure $tag -under 0]
		    $w tag bind $tag <ButtonRelease-1> \
			    "if {!\[info exists tk::Priv(mouseMoved)\] || !\$tk::Priv(mouseMoved)} \
			    {[list $OPT(edit) -attach [Attach] -type error -- $PRIV(errorInfo)]}"
		} else {
		    $w insert output $res\n stderr
		}
	    } elseif {$res ne ""} {
		$w insert output $res stdout \n stdout
	    }
	}
    }

    if { $IsTcl } {
        # for lisp, we show prompt after end of evaluation
        DoAfterCommand
    }
}



# procedure 
proc ::tkcon::EvalInSwankFromConsoleContinuation {w EventAsList cmd} {
    if {![winfo exists $w]} {
        # early abort - must be a deleted tab
        return
    }
            
    AddSlaveHistory $cmd

    if { [::mprs::Car [lindex $EventAsList 1]] ne {:ok} } {
        puts stderr "Something wrong: result is $EventAsList"        
    }

    # No need to do output - it is done when :write-string was processed
    # puts "::tkcon::EvalInSwankFromConsoleContinuation Event = $Event "

    DoAfterCommand
}


# Write a hyperlink to a text widget w
# problem is that we need mouse click to activate the hyperlink
proc ::tkcon::WriteActiveText {w text index code} {
    set tag [UniqueTag $w]
    $w tag configure $tag -foreground ForestGreen
    # $w insert output $text [list stdout $tag] \n stdout
    $w insert $index $text $tag  
    $w tag bind $tag <Enter> [list $w tag configure $tag -under 1]
    $w tag bind $tag <Leave> [list $w tag configure $tag -under 0]
    $w tag bind $tag <ButtonRelease-1> $code
}


# Passive text
# Do we really need unique tag here? FIXME (add some more tags like stderr)
proc ::tkcon::WritePassiveText {w text} {
    set tag [UniqueTag $w]
    $w tag configure $tag -foreground grey
    $w insert output $text\n [list stdout $tag]
}


proc ::tkcon::EditFileAtOffset {filename offset} {
    edit -type file -offset $offset $filename
    #see offset - incoroporate it into edit.
    # $w mark set insert "0.0+ $offset chars"
    # focus -force $editor
}


## ::tkcon::LispFindDefinition - clone of ::tkcon::Expand - 
# ARGS:	w	- text widget in which to expand str
# Calls:	::tkcon::Expand(Pathname|Procname|Variable)
# Outputs:	The string to match is expanded to the longest possible match.
#		If ::tkcon::OPT(showmultiple) is non-zero and the user longest
#		match equaled the string to expand, then all possible matches
#		are output to stdout.  Triggers bell if no matches are found.
# Returns:	number of matches found
## 
proc ::tkcon::LispFindDefinition {w {type ""}} {
    set exp "\[^\\\\\]\[\[ \t\n\r\\\{\"$\]"
    set tmp [$w search -backwards -regexp $exp insert-1c limit-1c]
    if {[string compare {} $tmp]} {append tmp +2c} else {set tmp limit}
    set str [$w get $tmp insert]
    LispFindDefinitionInner $str
}

    
