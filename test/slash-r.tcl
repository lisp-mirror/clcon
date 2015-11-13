# THis file was used to find some unsyncronization between # oduvanchik and clcon which in part was due to 
# <KeyPress> sometimes returned 13

package require Tk

proc CheckIfKeyPressIsNotReturn {a} {
    set acode -
    scan $a %c acode
	if {$acode == 13} {
        .txt insert end "\nJust Return - anomaly\n"
        .txt see insert
		bell
	}
}

# This is a modification of original binding from tk8.6/text.tcl 
bind Text <KeyPress> {
	CheckIfKeyPressIsNotReturn %A
	tk::TextInsert %W %A
}

bind Text <Return> {
	CheckIfKeyPressIsNotReturn %A
	tk::TextInsert %W %A
}


# This is a network i/o in real application
proc LongCalculation {n} {
  set result {}
  for {set i 0} {$i < $n} {incr i} {set result [lappend $result $i]}
  }

set global_counter 0 

text .txt
bind MyBindTag <Shift-Key-Return> { 
    LongCalculation 8000; 
	update;
	update idletasks;
	.txt insert end "Shift-Key-Return $global_counter - breaking\n"
	incr global_counter
	.txt see insert
	break 
}

bindtags .txt [linsert [bindtags .txt] 0 MyBindTag] 

pack .txt
focus .txt