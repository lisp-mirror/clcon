## Save file. Stolen from tcltextedit
namespace eval ::edt {
    proc FILESaveRequest {initialfile} {
        set non_canon [tk_getSaveFile -title {Save file} -initialfile $initialfile]
        return [CanonicalizeFileName $non_canon]
    }

    # FileName must be canonicalized, see ::edt::CanonicalizeFileName
    proc DoSaveFile { Bi clcon_text FileName Renaming} {
        # We force UNIX eol style and utf-8 encoding on all files we save
        set f [open $FileName w]
        fconfigure $f -translation {auto lf} -encoding utf-8
        puts $f [$clcon_text get 1.0 "end-1 char"] nonewline
        set mtime [file mtime $FileName]
        close $f 
        if {$Renaming} {
            $clcon_text edit modified 0
            EditCloseFile $Bi
            ::tkcon::DoOpenFileForEdit $FileName
        } else {
            ::mprs::AssertEq $FileName [[$clcon_text cget -opened_file] cget -filename]
            [$clcon_text cget -opened_file] configure -filemtime $mtime
            $clcon_text edit modified 0
        }
    }

    proc FileLessBufferP {clcon_text} {
        set opened_file [$clcon_text cget -opened_file]
        if {$opened_file eq {}} {
            return 1
        } elseif {[$opened_file cget -filemtime] eq {}} {
            return 1
        } else {
            return 0
        }
    }

    # Returns 1 if file was saved. Can modify clcon_text data.
    proc Save {Bi clcon_text} {
        set opened_file [$clcon_text cget -opened_file]
        set FileName [$opened_file cget -filename]
        if {[FileLessBufferP $clcon_text]} {
            SaveAs $Bi $clcon_text
        } elseif {![$clcon_text edit modified]} {
            tk_messageBox -parent $clcon_text -message "No changes to save"
            return 0
        } elseif {[$opened_file cget -filemtime] < [file mtime $FileName]} {
            tk_messageBox -parent $clcon_text -message "File have changed by another program. Can't save (yet)"
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
        showVar NewFileName

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
