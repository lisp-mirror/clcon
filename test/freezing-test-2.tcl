# Initialization (readonly set to 1)

catch { destroy .text400 }

toplevel .text400
set w .text400.tt
::clcon_text::clcon_text $w -readonly 1

# Changing readonly after creation
$w configure -readonly 0
$w configure -readonly 1

$w insert 1.0 "this line will be ignored"

for {set x 0} {$x<10} {incr x} { 
    $w RoInsert end "line $x\n"
}

# Synonyms
set w2 .text400.t2
::clcon_text::clcon_text $w2
$w2 insert end "Line inserted by 'insert'\n"
$w2 RoInsert end "Line inserted by 'RoInsert'\n"
::ro_out::I $w2 end "Line inserted by ::ro_out::I\n"

pack $w   -side top
pack $w2 -side bottom

