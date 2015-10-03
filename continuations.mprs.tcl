## Continuations infrastructure and functions
# Continuations are sheduled at tcl side to be invoked
# After swank reports some kind of return event with the same ContinuationId

# Initialize the ::mprs namespace (message parser)
# This is for SWANK communication-related stuff, though some parts are in ::tkcon namespace


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

## Continuations work for sync or async event
# Code accepts event in $EventAsList variable which contains event unleashed one time
proc ::mprs::EnqueueContinuation {ContinuationId code} {
    variable ContinuationsDict
    # Having two events in the queue is normal sometimes. 
    set PrintContinuationsDict [expr [llength $ContinuationsDict]>2]
    if {$code ne {}} {
        set code [list {EventAsList} $code]
    }
    dict set ContinuationsDict $ContinuationId $code
    if {$PrintContinuationsDict} {
        showVarPutd ContinuationsDict
    }
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
    # If we get error here, we trying to call continuation which was not sheduled or
    # which was lost
    set Continuation [dict get $ContinuationsDict $ContinuationId]
    dict unset ContinuationsDict $ContinuationId
    putd "RunContinuation $ContinuationId: Continuation = $Continuation"
    if {$Continuation eq {}} {
        return
    } else {
        apply $Continuation $EventAsList
    }
}
    
proc ::mprs::ExtractContinuationId {EventAsList} {
    set EventHead [lindex $EventAsList 0]
    if {[lsearch {:return :abort} $EventHead] >= 0} {
        return [Unleash [lindex $EventAsList 2]]               
    } else {
        return {}
    }
}

