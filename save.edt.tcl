## Save file. Stolen from tcltextedit
namespace eval ::edt {
    proc FILESaveRequest {initialfile} {
        set non_canon [tk_getSaveFile -title {Save file} -initialfile $initialfile -parent [::edt::theTW]]
        return [CanonicalizeFileName $non_canon]
    }

    # FileName must be canonicalized, see ::edt::CanonicalizeFileName
    proc DoSaveFile { Bi clcon_text FileName Renaming} {
        # We force UNIX eol style and utf-8 encoding on all files we save
        set f [open $FileName w]
        fconfigure $f -translation {auto lf} -encoding utf-8
        puts $f [$clcon_text get 1.0 "end-1c"] nonewline
        close $f 
        set mtime [file mtime $FileName]
        if {$Renaming} {
            $clcon_text edit modified 0
            ::tkcon::DoOpenFileForEdit $FileName
            EditCloseFile $Bi
            # We have no switch to buffer function, so we call ::edt::edit
            ::edt::edit -file -- $FileName
        } else {
            ::mprs::AssertEq $FileName [[$clcon_text cget -opened_file] cget -filename]
            [$clcon_text cget -opened_file] configure -filemtime $mtime
            $clcon_text edit modified 0
        }
        return 1
    }

    # Returns list of filename and explanation:
    # Explanation is "" if there is a file mapped to buffer,
    # or "no_file" if there is no file intended (e.g. error buffer)
    # or "new_file" if file was never saved.
    proc clcon_text_to_file_name {clcon_text} {
        set opened_file [$clcon_text cget -opened_file]
        if {$opened_file eq {}} {
            return {"" "no_file"} 
        } elseif {[$opened_file cget -filemtime] eq {}} {
            return {"" "new_file"}
        } else {
            return [list [$opened_file cget -filename] ""]
        }
    }

    proc FileLessBufferP {clcon_text} {
        lassign [clcon_text_to_file_name $clcon_text] filename reason
        if {$reason eq ""} {
            return 0
        } else {
            return 1
        }
    }
    

    # Returns 1 if file was saved. Can modify clcon_text data.
    proc Save {Bi clcon_text warn_if_does_not_need_save} {
        set opened_file [$clcon_text cget -opened_file]
        if {$opened_file eq {}} {
            set FileName {}
            set filemtime {}
        } else {
            set FileName [$opened_file cget -filename]
            set filemtime [$opened_file cget -filemtime]
        }
        if {$FileName eq {}} {
            SaveAs $Bi $clcon_text
        } elseif {$filemtime eq {}} {
            DoSaveFile $Bi $clcon_text $FileName 0
        } elseif {[catch { set real_mtime [file mtime $FileName]}]} {
            DoSaveFile $Bi $clcon_text $FileName 0
        } elseif {$filemtime < $real_mtime} {
            tk_messageBox -parent $clcon_text -message "File have changed by another program. Can't save (yet)"
            return 0
        } elseif {![$clcon_text edit modified]} {
            if {$warn_if_does_not_need_save} {
              tk_messageBox -parent $clcon_text -message "No changes to save"
            }
            return 0  
        } else {
            # There is a quant of time to modify file and harm us. 
            DoSaveFile $Bi $clcon_text $FileName 0
        }
    }


    # Returns 1 if file was saved. Can modify clcon_text data.
    proc SaveAs {Bi clcon_text} {
        set opened_file [$clcon_text cget -opened_file]
        set FileName [$opened_file cget -filename]
        # set UntitledBuffer 1
        set NewFileName [FILESaveRequest {}]

        if {$NewFileName eq {}} {
            return 0
        } elseif {[IsFileBeingEdited $NewFileName]} {
            tk_messageBox -parent $clcon_text -message "File is open in the editor already. Can't save to it"
            return 0
        } elseif {$FileName eq $NewFileName} {
            set Renaming 0
        } else {
            set Renaming 1
        }
        DoSaveFile $Bi $clcon_text $NewFileName $Renaming
    }
}
