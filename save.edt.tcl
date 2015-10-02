## Save file. Stolen from tcltextedit
namespace eval ::edt {
    proc FILESaveRequest {initialfile} {
        return [tk_getSaveFile -title {Save file} -initialfile initialfile]
    }
    
    proc DoSaveFile { clcon_text FileName } {
        set f [open $FileName w]
        puts $f [$clcon_text get 1.0 "end-1 char"] nonewline
        set mtime [file mtime $FileName]
        close $f
        [$clcon_text cget -opened_file] configure -filename $FileName -filemtime $mtime
        $clcon_text edit modified 0
    }


    proc UntitledBufferP {clcon_text} {
        set opened_file [$clcon_text cget -opened_file]
        expr {[$opened_file cget -filename]=={}}
    }

    proc DoSave {clcon_text FileName} {
    }

    # Returns 1 if file was saved. Can modify clcon_text data.
    proc Save {clcon_text} {
        set opened_file [$clcon_text cget -opened_file]
        set FileName [$opened_file cget -filename]
        if {[UntitledBufferP $clcon_text]} {
            set UntitledBuffer 1
            set FileName [FILESaveRequest {}]
            if {$FileName eq {}} {
                return 0
            }
        } elseif {![$clcon_text edit modified]} {
            tk_messageBox -parent $clcon_text -message "No changes to save"
            return 0
        } elseif {[$opened_file cget -filemtime] < [file mtime $FileName]} {
            tk_messageBox -parent $clcon_text -message "File have changed by another program. Can't save (yet)"
            return 0
        } else {
            # There is a quant of time to modify file and harm us. 
            set UntitledBuffer 0
        }
        DoSaveFile $clcon_text $FileName
        puts "see addrecent in tcltextedit/cmds.tcl"
    }
       
}
