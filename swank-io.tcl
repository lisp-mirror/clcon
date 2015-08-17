## Copyright (c) 1995-2011 Jeffrey Hobbs, jeff(a)hobbs(.)org
## Copyright (c) 2015 Denis Budyak
## MIT License

# Initialize the ::mprs namespace (message parser)
#
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

proc ::tkcon::FormatSwankRexEvalMessage {cmd ItIsListenerEval {ContinuationCounter {}}} {
    set ThreadDesignator [CalculateThreadDesignatorForSwank $ItIsListenerEval]
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


# tries to evaluate code in sync mode and returns a result
proc ::tkcon::EvalInSwankSync {lispcode} {
    #proc ::tkcon::FormatSwankRexEvalMessageInner {cmd ThreadDesignator ContinuationCounter} 
    variable SWANKIsInSyncMode
    SWANKIsInSyncMode = 1
    try {
        return [::mprs::EvalInSwankSyncInner $lispcode]
    } finally {
        mprs::DeleteSyncEventsFromTheQueue
        SWANKIsInSyncMode = 0
    }
}


# evaluates lispcode synchonously and returns a result (inner)
proc ::mprs::EvalInSwankSyncInner {lispcode} {
    variable ::tkcon::SWANKSyncContinuation
    set ::tkcon::SWANKSyncContinuation [GenContinationCounter]

}


## ItIsListenerEval must be 1 to wrap form into (swank-repl:listener-eval ...) or 0 otherwise
# If ContinuationCounter is not passed, it is calculated when needed
proc ::tkcon::EvalInSwankAsync {form {ItIsListenerEval 1} {ContinuationCounter {}}} {
    variable OPT
    variable PRIV

    #puts "entered EvalInSwank"

    # tcl escape: if lisp command starts from . , we (temporarily?) consider it as tcl escape
    if {[string index $form 0] eq "."} {
        tkcon main [string range $form 1 end]
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
    
    puts "regsub result: $cmd"

    set cmd [SwankMaybeWrapFormIntoListenerEval $cmd $ItIsListenerEval]

    puts "wrapped to listener eval: $cmd"
    
    #puts [list $PRIV(app) $cmd]
    
    set cmd [FormatSwankRexEvalMessage $cmd $ItIsListenerEval $ContinuationCounter]
    puts "About to send to SWANK: $cmd"

    # tr "About to send $cmd to SWANK"

    set code [catch {puts -nonewline $PRIV(app) $cmd ; flush $PRIV(app)} result]
        if {$code && [eof $PRIV(app)]} {
            ## Interpreter died or disappeared
            puts "$code eof [eof $PRIV(app)]"
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
    error "presentations are disabled in EMACS setup"
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

proc ::mprs::Unleash {x} {
    subst -nocommands -novariables [string range $x 1 end]
}

proc ::mprs::Unleash2 {x} {
    lindex [Unleash $x] 0
}

## minial testing facility
# tests are runned when code is loading (horrible!)
proc ::mprs::AssertEq {x y} {
    if {! ($x eq $y)} {
        error "Assertion failure: $x eq $y"
    }
}

proc ::mprs::Car {x} {
    if {[TypeTag $x] eq "l"} {
        lindex [Unleash $x] 0
    } else { error "Car: $x is not a list"
    }
}

proc ::mprs::Cadr {x} {
    if {[TypeTag $x] eq "l"} {
        lindex [Unleash $x] 1
    } else { error "Car: $x is not a list"
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

proc ::mprs::MaybeProcessSyncEvent {ContId EventAsList} {
    # find if it is in SWANKSyncContinuation
    # if found, process it
    # note if abort is called, we throw so we need another read loop for sync
    # messages. I think we mast configure -blocking 1 and put async messages into a queue
    # also we need global var to distinguish if we are in sync mode.
    if {$SWANKIsInSyncMode == 0} {
        return
    }
    return {}
}

# this is an async event. Process it. E.g. call a continuation
proc ::mprs::ProcessAsyncEvent {EventAsList} {
    set ContinuationId [ExtractContinuationId $EventAsList]
    puts "ProcessAsyncEvent Id=$ContinuationId Data=[Unleash [lindex $EventAsList 1]]"
    update
    return {}
} 

## ::mprs::ProcessEventsFromQueue
#  we are in async mode
#
proc ::mprs::ProcessEventsFromQueue {} {
    variable ::tkcon::SWANKEventQueue
    variable ::tkcon::SWANKIsInSyncMode
    while {($SWANKIsInSyncMode == 0)&&([llength $SWANKEventQueue] > 0)} {
        ProcessFirstEventFromQueue
    }
}

# we know there is an event on the queue. Lets process it.
proc ::mprs::ProcessFirstEventFromQueue {} {
    variable ::tkcon::SWANKEventQueue
    # Pop event 
    set Event [lindex $SWANKEventQueue 0]
    set SWANKEventQueue [lreplace $SWANKEventQueue 0 0]
    
    # We only understand list-shaped events
    AssertEq [TypeTag $Event] l
    set EventAsList [Unleash $Event]
    set EventHead [lindex $EventAsList 0]
    set ContinuationId [ExtractContinuationId $EventAsList]

    puts "ContinuationId = $ContinuationId"
    
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
        ProcessEventsFromQueue
        # return value makes no sence here
    }
}


## Temporary procedure to handle input from SWANK
## does not have counterparts in swank-protocol!!
proc ::tkcon::TempSwankChannelReadable {sock} {
    variable SWANKEventQueue
    variable SWANKIsInSyncMode

    # First of all we must have checked disconnect event, but let's skip it for now
    set Event [SwankReadMessageString]

    # just for debugging 
    puts "message from socket: $Event"

    if { [string index $Event 0] eq "(" } {
        puts "Skipping lisp-formed event $Event"
    } else {
        puts "queue is $SWANKEventQueue . Lets post to it"
        if { $SWANKIsInSyncMode == 0 } {
            lappend SWANKEventQueue $Event
            ::mprs::ProcessEventsFromQueueIfAppropriate
        } else {
            lappend SWANKEventQueue $Event
            # Do nothing more. Sync negotiation loop would vwait this var
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
proc ::tkcon::SetupSwankConnection {channel} {
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

    # puts is for debugging here
    puts [SwankReadMessage]

    # Start it up
    # disabled at emacs SwankRequestInitPresentations

    SwankRequestCreateRepl

    # Wait for startup
    # puts is for debugging here
    puts [SwankReadMessage]

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

    if {[llength [info level 0]] == 1} {
	# no args were specified, return the attach info instead
	return [AttachId]
    }
    set path [concat $PRIV(name) $OPT(exec)]

    set PRIV(displayWin) .
    global tcl_version
    if {[catch {eof $name} res]} {
        return -code error "No known channel \"$name\""
    } elseif {$res} {
        catch {close $name}
        return -code error "Channel \"$name\" returned EOF"
    }
    set app $name
    set type swank

    if {![info exists app]} { set app $name }

    # SwankThread is like swank-protocol::connection thread slot
    array set PRIV [list app $app appname $name apptype $type deadapp 0 SwankThread t]

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
    fconfigure $name -buffering full -blocking 0
    
    # It is important we initialize connection before binding fileevent
    SetupSwankConnection $name

    # The file event will just puts whatever data is found
    # into the interpreter
    # fileevent $name readable [list ::tkcon::EvalSocketEvent $name]
    fileevent $name readable [list ::tkcon::TempSwankChannelReadable $name]
    
    return [AttachId]
}

proc ::tkcon::OuterNewSwank {} {
    ::tkcon::NewSwank 127.0.0.1 4009
}

## ::tkcon::NewSwank - called to create a socket to connect to
# ARGS:	none
# Results:	It will create a socket, and attach if requested
##
proc ::tkcon::NewSwank {host port} {
    variable PRIV
    if {[catch {
	set sock [socket $host $port]
        fconfigure $sock -encoding utf-8
    } err]} {
	tk_messageBox -title "Socket Connection Error" \
		-message "Unable to connect to \"$host:$port\":\n$err" \
		-icon error -type ok
    } else {
	AttachSwank $sock
    }
}



## ::tkcon::ExpandLispSymbol (currently known as ExpandProcname)
# - expand a lisp symbol based on $str
# ARGS:	str	- partial proc name to expand
# Used to Call:	        ::tkcon::ExpandBestMatch
# Used to Return:	list containing longest unique match followed by all the
#		possible further matches
##

proc ::tkcon::testProc {a1 a2} {
   puts $a1
   puts $a2
}
proc ::tkcon::ExpandLispSymbol str {

    
    # require at least a single character, otherwise continue
    if {$str eq ""} {return -code continue}
    
    set LispCmd {(subseq (format nil "~{ ~A~}" (first (swank:simple-completions "a" "COMMON-LISP-USER"))) 1)}
    
    #проблема 1 - похоже, нас не понимает tcl - не видит, что это один аргумент.
    #проблема 2 - нужно выполнить синхронно и вернуть результат, обыыный \EvalAttached' - асинхронный
    
    testProc $LispCmd 1
    #puts "Ok"
    #tr [alias]
    set SwankReply [::tkcon::EvalInSwankSync "$LispCmd" 0]
    tr "Passed EvalInSwank"

    puts $SwankReply
    
    set match [list $SwankReply]

    if {[llength $match] > 1} {
	regsub -all {([^\\]) } [ExpandBestMatch $match $str] {\1\\ } str
	set match [linsert $match 0 $str]
    } else {
	regsub -all {([^\\]) } $match {\1\\ } match
    }
    return -code [expr {$match eq "" ? "continue" : "break"}] $match
}
