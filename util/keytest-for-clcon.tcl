package require Tk
set keysym "Press any key"
toplevel .keytest
pack [label .keytest.l -textvariable keysym -padx 2m -pady 1m]
bind .keytest <Key> {
    set keysym "You pressed %k %K %s %A"
    puts "%A %K"
}
