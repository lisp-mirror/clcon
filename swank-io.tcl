## Copyright (c) 1995-2011 Jeffrey Hobbs, jeff(a)hobbs(.)org
## Copyright (c) 2015 Denis Budyak
## MIT License

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

proc ::tkcon::FormatSwankRexEvalMessage {cmd ItIsListenerEval} {
    set ContinuationCounter [GenContinuationCounter]
    set ThreadDesignator [CalculateThreadDesignatorForSwank $ItIsListenerEval]
    # commented out line is for my patched version which passes readtable
    # set msgNoLen "(:emacs-rex-rt $cmd \"COMMON-LISP-USER\" nil $ThreadDesignator $ContinuationCounter)"
    set msgNoLen "(:emacs-rex $cmd \"COMMON-LISP-USER\" $ThreadDesignator $ContinuationCounter)"
    set strLenHex [format "%06X" [string length $msgNoLen]]
    set msgAndLen [string cat $strLenHex $msgNoLen]
    return $msgAndLen
}

proc ::tkcon::SwankMaybeWrapFormIntoListenerEval {form ItIsListenerEval} {
    if {$ItIsListenerEval == 1} {
        set cmd [regsub -all {([\"\\])} $form {\\\0}]
        return "(swank-repl:listener-eval \"$cmd\")"
    } else {
        return $form
    }
}

## ItIsListenerEval must be 1 to wrap form into (swank-repl:listener-eval ...) or 0 otherwise
proc ::tkcon::EvalInSwank {form {ItIsListenerEval 1}} {
    variable OPT
    variable PRIV

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
    
    set cmd [FormatSwankRexEvalMessage $cmd $ItIsListenerEval]
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
    EvalInSwank $form $ItIsListenerEval
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

## Temporary procedure to handle input from SWANK
## does not have counterparts in swank-protocol!!
proc ::tkcon::TempSwankChannelReadable {sock} {
    set Message [SwankReadMessageString]

    # just for debugging 
    puts "message from socket: $Message"

    if { [string index $Message 0] eq "!" } {
        set cmd [string range $Message 1 end]
        eval $cmd
        return
    } else {
        return $Message
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

    # this is not from lime!
    SwankNoteTclConnection 

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

    interp alias {} ::tkcon::EvalAttached {} ::tkcon::EvalInSwank {} \#0
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



