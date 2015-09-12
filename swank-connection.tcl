## Swank connection object

namespace eval ::swcnn {
    variable SwankConnectionCounter 0

    # name of connection
    variable CurrentSwankConnection {}
}


proc ::swcnn::GenSwankConnectionCounter {} {
    variable SwankConnectionCounter
    
 if {![info exists SwankConnectionCounter]} {
    set SwankConnectionCounter 1
 } else {
    set SwankConnectionCounter [expr {$SwankConnectionCounter + 1}]
 }
 return $SwankConnectionCounter
}

# Throws an error if can't close sock
# Deletes the connection
proc ::swcnn::TerminateConnection {name} {
    variable CurrentSwankConnection
    upvar \#0 $name con

    if { $CurrentSwankConnection eq $name } {
        set CurrentSwankConnection {}
    }
    if {[lsearch {socket_only initialized} $con(state)]} {
        set sock $con(sock)
        unset $name
        close $sock
    } else {
        unset $name
    }
}

proc ::swcnn::TerminateCurrentConnection {} {
    variable CurrentSwankConnection
    if { $CurrentSwankConnection ne {} } {
        TerminateConnection $CurrentSwankConnection
    }
}


# connection: an array variable named SwankConnectionNNN
# array members:
# sock - channel
# state 
#   new
#   socket_only
#   initialized
#   dead (socket eof)
# SWANKIsInSyncMode? we only support one SWANK now, so don't matter

# instead of ::tkcon::NewSwank
# Results:	It will create a socket, and attach console to it
# FIXME console vs interpreter - can we have one interpreter (or one swank)
# for several tabs? This is noncence, but what does tkcon think?
# We assume that console is attached to slave interpreter already
proc ::swcnn::MakeSwankConnection {host port} {
    variable CurrentSwankConnection
    variable ::tkcon::PRIV

    if {$::swcnn::CurrentSwankConnection ne {}} {
        error "::swcnn::MakeSwankConnection: connected already (1)"
    }            

    if {$CurrentSwankConnection ne {}} {
        error "::swcnn::MakeSwankConnection: connected already (2)"
    }            

    
    TerminateCurrentConnection

    set id [GenSwankConnectionCounter]
    set name "::swcnn::SwankConnection$id"
    variable $name
    upvar \#0 $name con
    array set con {state new}
    #set con(sock) 123123213213
    #puts $con(sock)
    if {[catch {
        set sock [socket $host $port]
	set con(sock) $sock
        fconfigure $sock -encoding utf-8
        set con(state) socket_only
    } err]} {
	tk_messageBox -title "Socket Connection Error" \
		-message "Unable to connect to \"$host:$port\":\n$err" \
            -icon error -type ok
    } else {
        set CurrentSwankConnection $name
	::tkcon::AttachSwank $name
    }
}
