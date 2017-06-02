set keysym "*********  [encoding system] *********"
encoding system utf-8
package require Tk
pack [label .l -textvariable keysym -padx 2m -pady 1m]
bind . <Key> {
    set keysym "You pressed %k %K %s %A"
}
