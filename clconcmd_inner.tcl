## clcon project - IDE commands internals

namespace eval ::clconcmd_inner {
    proc finf_inner {dirs types string} {
        set lisp_globs ""        
        foreach dir $dirs {
            if {[string range $dir end end] ne "/"} {
                set dir [string cat $dir "/"]
                foreach type $types {
                    set glob [string cat $dir "**/*." $type]
                    set qGlob [::tkcon::QuoteLispObjToString $glob]
                    set lisp_globs [string cat $lisp_globs " " $qGlob]
                }
            }
        }


        set qString [::tkcon::QuoteLispObjToString $string]
        set lisp_cmd "(clco:find-string-in-files $qString
  (clco:FILES-BY-GLOB-LIST $lisp_globs))"
       
        ::tkcon::FocusConsole
        ::tkcon::SendEventToSwank $lisp_cmd {puts ""} 1 t
    }
}

 

        

