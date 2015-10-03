## clcon project - IDE commands
## (c) Denis Budyak 2015
## MIT License

namespace eval clconcmd {
    proc help {} {
    set result1 [info procs ::clconcmd::*]
    regsub -all "::clconcmd::" $result1 "" result2
    puts $result2
    }
    
    #proc history {} {
    #    history
    #}

    proc tcsoh {filename} {
        tkcon main TkconSourceHere $filename
    }

    proc bufferlist {} {
        tkcon main ::buli::BufferListBox 
    }

    proc history {} {
        tkcon main history
    }

    # Run oduvanchik command (with 
    proc o {commandNameWoPrefix} {
        ::edt::oImplementation $commandNameWoPrefix
    }

    proc erbr {} {
        tkcon main ::erbr::SwankBrowseErrors1 {'defun}
    }

    proc insp* {} {
        tkcon main ::insp::SwankInspect "*"
    }

    proc ed {filename} {
        ::edt::edit -type file $filename
    }
}
    
