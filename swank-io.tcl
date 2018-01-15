## Copyright (c) 1995-2011 Jeffrey Hobbs, jeff(a)hobbs(.)org
## Copyright (c) 2015 Denis Budyak
## MIT License

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

namespace eval ::mprs {
    variable ContinuationsDict {}
}

# Load conses
TkconSourceHere continuations.mprs.tcl
TkconSourceHere send-to-swank.tcl

proc ::tkcon::myerror {text} {
    idebug on
    puts stderr "myerror: $text" 
    tk_messageBox -title "myerror" -message $text
    idebug break
    idebug off
}

# e.g. .puts [::tkcon::EvalInSwankSync {(swank:simple-completions "sw" '"COMMON-LISP-USER")}]

# tries to evaluate code in sync mode and returns a result
# Can call inner event loop, but no async events are processed.
# If some async event handler is on the stack already and sheduled events,
# we can't help it. 
proc ::tkcon::EvalInSwankSync {lispcode {window {}}} {
    #proc ::tkcon::FormatSwankRexEvalMessageInner {cmd ThreadDesignator ContinuationCounter} 
    variable SWANKIsInSyncMode
    variable SWANKSyncContinuation
    if { $SWANKIsInSyncMode == 1 } {
        myerror "::tkcon::EvalInSwankSync is not reenterable"
    }
    set SWANKIsInSyncMode 1
    if {$window ne {}} {grab $window}
    try {
        return [::mprs::EvalInSwankSyncInner $lispcode]
    } finally {
        if {$window ne {}} {grab release $window}
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
    ::tkcon::EvalInSwankAsync $lispcode {} t $ContinuationCounter

    # Delay all async events from lisp. Process sync events. 
    while {1 == 1} {

        # putd "EvalInSwankSyncInner goes vwait for SWANKEventQueue"

        vwait ::tkcon::SWANKEventQueue

        # putd "EvalInSwankSyncInner woke up on vwait SWANKEventQueue"

        set processed [MaybeProcessSyncEventFromQueue]

        # putd "MaybeProcessSyncEventFromQueue returned $processed"
        if { [lindex $processed 0] == 1} {
            set result [lindex $processed 1]
            # putd "EvalInSwankSyncInner: about to return $result"
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


## swank-protocol::request-swank-require
proc ::tkcon::SwankRequestSwankRequire1 {requirement continuation} {
    ::tkcon::SSCSend "(swank:swank-require '$requirement)" $continuation
}

## swank-protocol::request-init-presentations
proc ::tkcon::SwankRequestInitPresentations {} {
    myerror "presentations are disabled in EMACS setup"
    ::tkcon::SSCSend "(swank:init-presentations)" {}
}

proc ::tkcon::SwankNoteTclConnection {continuation} {
    ::tkcon::SSCSend "(clco:note-this-is-tcl-connection)" $continuation
}

proc ::tkcon::SwankReplReady {EventAsList} {
    variable PRIV
    ::tkcon::ChangeCurrentPackageA $EventAsList 
    set PRIV(SwankThread) 1
    set PRIV(SwankReplReady) 1
}


proc ::tkcon::SwankRequestCreateRepl {continuation} {
    variable PRIV
    ::tkcon::SSCSend {(swank-repl:create-repl nil :coding-system "utf-8-unix")} \
        $continuation
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

        # putd "Checking if $Event is sync = $SWANKSyncContinuation"
        
        AssertEq [Consp $Event] 1
        # putd "Passed AssertEq"
        set EventAsList [Unleash $Event]
        set ContinuationId [ExtractContinuationId $EventAsList]
        if { $ContinuationId == $SWANKSyncContinuation } {
            set SWANKEventQueue [lreplace $SWANKEventQueue $i $i]
            return $Event
        }
    }
    return {}
}


# Parses (:return (:ok x)) event to message or errs.
# Returns unleashed x if all ok
# Если в событии пришёл nil, то мы безсильны. Если нужно проверить на nil, разбирайте событие заново
# или пишите ф-ю, к-рая не будет делать Unleash
proc ::mprs::ParseReturnOk { EventAsList } {
    set EventHead [lindex $EventAsList 0]
    if { $EventHead ne {:return} } {
        puts stderr "Something wrong: in SWANK reply we expected :return, but get $EventHead"
    }
    set SwankReply [::mprs::Unleash [lindex $EventAsList 1]]
    set HeadSwankReply [lindex $SwankReply 0]
    if {$HeadSwankReply eq {:ok}} {
        set result [::mprs::Unleash [lindex $SwankReply 1]]
        return $result
    } else {
        error "I don't know what is $HeadSwankReply while parsing swank reply $EventAsList"
    }
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

    # putd "EventHead = $EventHead"

    if {$EventHead eq ":abort"} {
        putd "Processing of :abort sync return is not done yet, throwing"
        throw "abort of sync eval"
    } elseif {$EventHead eq ":return"} {
        set Body [lindex $EventAsList 1]
        # putd "Body = $Body"
        return [list 1 $Body]
    } else {
        putd "Unknown head $EventHead in sync reply $Event"
    }
    # assume event is processed (if it is not an :abort)
    return [list 1 $Event]
}


proc ::mprs::EvalInTclSync {EventAsList} {
    set ThreadId [Unleash [lindex $EventAsList 1]]
    set Tag [Unleash [lindex $EventAsList 2]]
    set Code [Unleash [lindex $EventAsList 3]]
    # putd "567989 Entered EvalInTclSync with $Tag, $Code"
    set Result [eval $Code]
    # set errorCode [catch {eval $Code} Result]
    # if {$errorCode} {
    #     tk_messageBox -message "6367: Error in EvalInTclSync - don't know what to do. See clco:eval-in-tcl"
    #     return
    # }
    set qResult [::tkcon::QuoteLispObjToString $Result]
    set EvalInTclValueForm "(:ok $qResult)"
    #showVar EvalInTclValueForm
    #puts stderr "Returning from EvalInTclSync"
    ::tkcon::SendEventToSwank $EvalInTclValueForm {} 3 $ThreadId $Tag
    # putd "567989 Returning from EvalInTclSync with $Tag"
    return     
}

proc ::mprs::DoReadString {EventAsList} {
    set ThreadId [Unleash [lindex $EventAsList 1]]
    set Tag [Unleash [lindex $EventAsList 2]]

    set _ok ""
    set t ".inputString_$Tag"
    toplevel $t
    wm title $t "Введите строку"
    wm protocol $t WM_DELETE_WINDOW "set _ok 0"
    set f1 [frame $t.f1]
    set f2 [frame $t.f2]
    pack $f1 -side top -expand 1 -fill both
    pack $f2 -side top -expand 1 -fill both

    set f [frame $f1.f]
    entry $f.e
    pack $f -side top -expand 1 -fill both
    pack $f.e -side left -expand 1 -fill both

    button $f2.bOk -text "OK" -command "set _ok 1"
    button $f2.bCancel -text "Отмена" -command "set _ok 0"

    bind $t <Return> "set _ok 1"

    pack $f2.bOk $f2.bCancel -side left
    focus $f1.f.e

    #wait for button
    vwait _ok
    if {$_ok==0} {
        destroy $t
        set tmp ""
    } else {
        set tmp [$f1.f.e get]
    }
    ::tkcon::SendEventToSwank "(:emacs-return-string $ThreadId $Tag \"$tmp\n\")" {} 2
    if {[winfo exists $t]} {
        destroy $t
    }
}

# this is an async event received from swank. Process it. E.g. call a continuation
# Я точно не помню, но похоже, что swank отправляет его через swank::send-to-emacs 
proc ::mprs::ProcessAsyncEvent {EventAsList} {
    set ContinuationId [ExtractContinuationId $EventAsList]
    # we don't need to Unleash keywords
    set Head [Unleash [lindex $EventAsList 0]]
    if { $Head eq ":write-string" } {
        # There is also a third element in the list, e.g. :repl-result
        puts -nonewline [Unleash [lindex $EventAsList 1]]
        ::tkcon::SheduleCheckSWANKEventQueue
    } elseif { $Head eq ":eval-no-wait" } {
        # is it a synchronous evaluation indeed?
        eval [Unleash [lindex $EventAsList 1]]
    } elseif { $Head eq  ":eval"} {
        EvalInTclSync $EventAsList
    } elseif { $Head eq ":debug" } {
        ::ldbg::StartDebugging $EventAsList
    } elseif { $Head eq ":debug-activate" } {
        ::ldbg::DebugActivate $EventAsList
    } elseif { $Head eq ":debug-return" } {
        ::ldbg::DebugReturn $EventAsList $ContinuationId
    } elseif { $Head eq ":indentation-update" } {
        putd "::mprs::ProcessAsyncEvent: impolitely ignoring :indentation-update"
    } elseif { $Head eq ":new-package" } {
        ::tkcon::ChangeCurrentPackageB $EventAsList       
    } elseif { $Head eq ":new-package-rt" } {
        # ::showVar EventAsList
        ::tkcon::ChangeCurrentPackageB $EventAsList       
    } elseif { $Head eq ":new-features" } {
        putd "::mprs::ProcessAsyncEvent: ignoring new features event: $EventAsList"
    } elseif { $Head eq ":ed" } {
        ::edt::ProcessEdRequest $EventAsList
    } elseif { $Head eq ":ping" } {
        ::swcnn::Pong $EventAsList
    } elseif { $Head eq ":read-string" } {
        ::mprs::DoReadString $EventAsList
    } elseif { [ContinuationExistsP $ContinuationId ] == 1 && [lsearch -exact [list ":return"] $Head] >= 0 } {
        # we should have generated event which would evaluate continuation later.
        # but what we will do with sync events then?
        # we must either run all continations asynchronously, either run all continuations synchronously.
        # putd "About to RunContinuation for $ContinuationId"
        RunContinuation $ContinuationId $EventAsList
    } else {
        puts stderr "::mprs::ProcessAsyncEvent: skipping async event from lisp: $EventAsList" 
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

    # putd "ContinuationId = $ContinuationId"
    
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


## Procedure to handle input from SWANK
## does not have counterparts in swank-protocol!!
proc ::tkcon::SwankChannelReadable {sock} {
    variable SWANKEventQueue
    variable SWANKIsInSyncMode

    set Event [SwankReadMessageString]

    # just for debugging 
    # putd "217901 message from socket: $Event"

    if { [string index $Event 0] eq "(" } {
        puts stderr "Skipping lisp-formed event $Event"
    } else {
        if {[llength $SWANKEventQueue]} {
            putd "queue is $SWANKEventQueue . Lets post to it"
        }
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
    set DecodedString [encoding convertfrom utf-8 $Buffer]

    if {[eof $stream]} {
        EvalSocketClosed $stream
        return
    }
    
    return $DecodedString
}

## from swank-protocol::read-message-string
proc ::tkcon::SwankReadMessageString {} {
    variable ::swcnn::CurrentSwankConnection
    variable PRIV

    if {$CurrentSwankConnection eq {}} {
        error "Attempt to read from closed connection"
    }

    upvar \#0 $CurrentSwankConnection con

    
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
    return [::tkcon::SwankReadMessageString]
}


## Modelled after defmethod lime::connect :after
## some parts are not implemented yet
proc ::tkcon::SetupSwankConnection {channel console continuation} {

    # this is tcl/tk specific
    bind $console <<CheckSWANKEventQueue>> ::mprs::ProcessEventsFromQueueIfAppropriate

    # this is not from lime!
    ::tkcon::SwankNoteTclConnection "::tkcon::SSC2 \$EventAsList [list $continuation]"
}


proc ::tkcon::SSC2 {EventAsList continuation} {
    # We have nothing to do with this reply
    putd EventAsList

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
  ::tkcon::SwankRequestSwankRequire1 "swank-repl" "::tkcon::SSC3 \$EventAsList [list $continuation]"
}


proc ::tkcon::SSC3 {EventAsList continuation} {
    # nothing to do with the reply
    putd $EventAsList

    # Start it up
    # disabled at emacs SwankRequestInitPresentations

    ::tkcon::SwankRequestCreateRepl "::tkcon::SSC4 \$EventAsList [list $continuation]"
}

proc ::tkcon::SSC4 {EventAsList continuation} {
    # Wait for startup. We read message here and set some variables from it.
    ::tkcon::SwankReplReady $EventAsList 
    ::tkcon::SSC5 $continuation
}

proc ::tkcon::SSC5 {continuation} {
    #;; Read all the other messages, dumping them
    #(swank-protocol:read-all-messages connection))
    #
    set con(state) "AttachSwank : initialized"

    eval $continuation
}

proc ::tkcon::DisconnectFromSwank {} {
    variable PRIV
    variable ::mprs::ContinuationsDict 
    set name $::swcnn::CurrentSwankConnection
    if {$name eq {}} {
        error "::tkcon::DisconnectFromSwank: disconnected already"
    }
    set ::swcnn::CurrentSwankConnection {}
    set ContinuationsDict [dict create]
    ::swcnn::TerminateConnection $name
    ::edt::OduvanchikDisconnected
    Prompt
}             


proc ::tkcon::ПринудительноОстановитьСерверТекущегоПодключенияSwank {{molcha 1}} { 
    set name $::swcnn::CurrentSwankConnection
    if {$name eq {}} {
        if {$molcha != 1} {
            error "Нет текущего подключения Swank"
        }
    } else {
        if {$molcha != 1} {
            puts "Пытаемся остановить лисп, обслуживающий подключение Swank, отправив в SBCL вызов (sb-ext:exit :abort t). Удастся ли это? Всё зависит от того, насколько сломан текущий сервер Swank, образ SBCL и связь с сервером"
        }
        ::tkcon::EvalInSwankAsync "(clco:terminate-lisp)" {} t 
    }
}    


## ::tkcon::AttachSwank - called to setup SWANK connection
# ARGS:	
#  name         - swank connection name  
#  continuation - a parameterless body to call after connection
# Results:	::tkcon::EvalAttached is recreated to send commands to socket
##
proc ::tkcon::AttachSwank {name continuation} {
    variable PRIV
    variable OPT
    variable ATTACH
    variable $name

    set PRIV(SwankReplReady) 0

    upvar \#0 $name con
    set sock $con(sock)
    # puts stderr "WARNING! tkcon allows for several consoles, but do not try to have more than one SWANK attachment simultanously" 

    if {[llength [info level 0]] == 1} {
	# no args were specified, return the attach info instead
	error "Something wrong in AttachSwank"
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
    set ::swcnn::CurrentSwankConnection $name

    if { $con(state) eq "socket_only" } {
        fconfigure $sock -buffering full -blocking 0
    
        fileevent $sock readable [list ::tkcon::SwankChannelReadable $sock]
        
        ::tkcon::SetupSwankConnection $sock $PRIV(console) [list ::tkcon::AttachSwankTail $continuation]

        return

    } elseif { $con(state) eq "initialized" } {
        # do nothing
        puts stderr "Initialized already"
        # We can't reach this point as we call AttachSwank only from OuterNewSwank
    } else {
        myerror "Unexpected state $con(state)"
    }

    ::tkcon::AttachSwankTail $continuation
}


proc ::tkcon::AttachSwankTail {continuation} {
    variable PRIV
    variable ATTACH
    ::mprs::AssertEq $PRIV(SwankReplReady) 1
    WritePassiveText $PRIV(console) "Connected to swank" output


    # Для запуска файлов из командной строки
    if {[info proc ::AttachSwankHook] != {}} {
       ::AttachSwankHook
    }    

    Prompt
    eval $continuation
}



proc ::tkcon::OuterNewSwank {} {
    # ::tkcon::NewSwank 127.0.0.1 4009
    variable OPT
    variable PRIV
    WritePassiveText $PRIV(console) "Connecting to swank..." output
    ::swcnn::MakeSwankConnection $OPT(swank-ip) $OPT(swank-port)
}


