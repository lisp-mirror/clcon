catch { destroy .freezingExample }
toplevel .freezingExample

set txt .freezingExample.text
::clcon_text::clcon_text $txt

for {set x 0} {$x<10} {incr x} { 
    $txt insert end "line $x\n"
}

pack $txt -side top -fill both
focus $txt

# Do .freezingExample.text Freeze
# to start buffering input
# Do .freezingExample.text Unfreeze
# to finish
