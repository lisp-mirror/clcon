## Unknown changed to get output into tkcon window
# unknown:
# Invoked automatically whenever an unknown command is encountered.
# Works through a list of "unknown handlers" that have been registered
# to deal with unknown commands.  Extensions can integrate their own
# handlers into the 'unknown' facility via 'unknown_handler'.
#
# If a handler exists that recognizes the command, then it will
# take care of the command action and return a valid result or a
# Tcl error.  Otherwise, it should return "-code continue" (=2)
# and responsibility for the command is passed to the next handler.
#
# Arguments:
# args -	A list whose elements are the words of the original
#		command, including the command name.
proc unknown args {
    global unknown_handler_order unknown_handlers errorInfo errorCode

    #
    # Be careful to save error info now, and restore it later
    # for each handler.  Some handlers generate their own errors
    # and disrupt handling.
    #
    set savedErrorCode $errorCode
    set savedErrorInfo $errorInfo

    if {![info exists unknown_handler_order] || \
	    ![info exists unknown_handlers]} {
	set unknown_handlers(tcl) tcl_unknown
	set unknown_handler_order tcl
    }

    foreach handler $unknown_handler_order {
        set status [catch {uplevel 1 $unknown_handlers($handler) $args} result]

        if {$status == 1} {
            #
            # Strip the last five lines off the error stack (they're
            # from the "uplevel" command).
            #
            set new [split $errorInfo \n]
            set new [join [lrange $new 0 [expr {[llength $new]-6}]] \n]
            return -code $status -errorcode $errorCode \
                -errorinfo $new $result

        } elseif {$status != 4} {
            return -code $status $result
        }

        set errorCode $savedErrorCode
        set errorInfo $savedErrorInfo
    }

    set name [lindex $args 0]
    return -code error "invalid command name \"$name\""
}

# tcl_unknown:
# Invoked when a Tcl command is invoked that doesn't exist in the
# interpreter:
#
#	1. See if the autoload facility can locate the command in a
#	   Tcl script file.  If so, load it and execute it.
#	2. If the command was invoked interactively at top-level:
#	    (a) see if the command exists as an executable UNIX program.
#		If so, "exec" the command.
#	    (b) see if the command requests csh-like history substitution
#		in one of the common forms !!, !<number>, or ^old^new.  If
#		so, emulate csh's history substitution.
#	    (c) see if the command is a unique abbreviation for another
#		command.  If so, invoke the command.
#
# Arguments:
# args -	A list whose elements are the words of the original
#		command, including the command name.

proc tcl_unknown args {
    global auto_noexec auto_noload env unknown_pending tcl_interactive
    global errorCode errorInfo
    #tk_messageBox -title "tcl_unknown" -message "$args"
    puts "tcl_unknown $args"

    # If the command word has the form "namespace inscope ns cmd"
    # then concatenate its arguments onto the end and evaluate it.

    set cmd [lindex $args 0]

    # Save the values of errorCode and errorInfo variables, since they
    # may get modified if caught errors occur below.  The variables will
    # be restored just before re-executing the missing command.

    set savedErrorCode $errorCode
    set savedErrorInfo $errorInfo
    set name [lindex $args 0]
    if {![info exists auto_noload]} {
	#
	# Make sure we're not trying to load the same proc twice.
	#
	if {[info exists unknown_pending($name)]} {
	    return -code error "self-referential recursion in \"unknown\" for command \"$name\""
	}
	set unknown_pending($name) pending
	if {[llength [info args auto_load]]==1} {
	    set ret [catch {auto_load $name} msg]
	} else {
	    set ret [catch {auto_load $name [uplevel 1 {namespace current}]} msg]
	}
	unset unknown_pending($name)
	if {$ret} {
	    return -code $ret -errorcode $errorCode \
		    "error while autoloading \"$name\": $msg"
	}
	if {![array size unknown_pending]} { unset unknown_pending }
	if {$msg} {
	    set errorCode $savedErrorCode
	    set errorInfo $savedErrorInfo
	    set code [catch {uplevel 1 $args} msg]
	    if {$code ==  1} {
		#
		# Strip the last five lines off the error stack (they're
		# from the "uplevel" command).
		#

		set new [split $errorInfo \n]
		set new [join [lrange $new 0 [expr {[llength $new]-6}]] \n]
		return -code error -errorcode $errorCode \
			-errorinfo $new $msg
	    } else {
		return -code $code $msg
	    }
	}
    }
    # at least we know that the problem exists
    puts "tcl_unknown failed to help us with $args"
    # tk_messageBox -title "tcl_unknown" -message "failed with $args"
    return -code continue
}

