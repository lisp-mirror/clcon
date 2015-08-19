## Swank connection object

namespace eval ::swcnn {
    ## two level array of connections
    #variable Connections
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

# instead of ::tkcon::NewSwank
# Results:	It will create a socket, and attach console to it
# FIXME console vs interpreter - can we have one interpreter (or one swank)
# for several tabs? This is noncence, but what does tkcon think?
proc ::swcnn::MakeSwankConnection {host port} {
    set id [GenSwankConnectionCounter]
    set name "::swcnn::SwankConnection$id"
    variable $name
    upvar \#0 $name con
    array set $name {}
    #set con(sock) 123123213213
    #puts $con(sock)
    if {[catch {
        set sock [socket $host $port]
	set con(sock) $sock
        fconfigure $sock -encoding utf-8
    } err]} {
	tk_messageBox -title "Socket Connection Error" \
		-message "Unable to connect to \"$host:$port\":\n$err" \
            -icon error -type ok
    } else {
	::tkcon::AttachSwank $name
    }
}
