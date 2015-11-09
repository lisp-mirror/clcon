## editor - part which is independent on window management.

namespace eval ::edt {
    proc CanonicalizeFileName {name} {
        global tcl_platform
        if {$tcl_platform(platform) == "windows"} {
            set name [::WindowsFileNameToUnix $name]
            return [string tolower $name]
        } else {
            return $name
        }
    }

    # This function increases ReuseCounter for non-reusable parameter sets
    # Take care to call it once for every parameter set.
    proc CanonicalizeEditArgs {word opts} {
        set type [dict get $opts -type]
        if {$type eq "file"} {
            set word [CanonicalizeFileName $word]
            return [list $word -type $type]
        } elseif {$type eq "newfile"} {
            set word "untitled [::edt::GenReuseCounter]"
            return [list $word -type $type]
        } elseif {$type eq "error"} {
            return [list tcl_error -type $type -no [GenReuseCounter]]
        } else {
            # never reuse procs, vars, errors
            return [list $word -type $type -no [GenReuseCounter]]
        }
    }

    # Parses line and returns list of {word opts tail}
    proc EditorParseArgs {args} {
        variable ::tkcon::PRIV
        variable ::tkcon::COLOR
        variable ::tkcon::OPT

        set args [lindex $args 0]
        
        set opts [dict create -find {} -type {} -attach {} -wrap {none} -offset {}]             
        while {[string match -* [lindex $args 0]]} {
            switch -glob -- [lindex $args 0] {
                -f*	{ dict set opts -find [lindex $args 1] }
                -a*	{ dict set opts -attach [lindex $args 1] }
                -t*	{ dict set opts -type [lindex $args 1] }
                -w*	{ dict set opts -wrap [lindex $args 1] }
                -o* { dict set opts -offset [lindex $args 1] }
                --	{ set args [lreplace $args 0 0]; break }
                default {return -code error "unknown option \"[lindex $args 0]\""}
            }
            set args [lreplace $args 0 1]
        }

        # determine what interpreter we are dealing with (broken)
        # if {[llength [ dict get $opts -attach]]} {
        #     foreach {app type} [ dict get $opts -attach] {break}
        # } else {
        #     foreach {app type} [tkcon attach] {break}
        # }
        
        foreach {app type} [tkcon attach] {break}
        
        set word [lindex $args 0]

        if {[dict get $opts -type] == {}} {
            if {[llength [::tkcon::EvalOther $app $type info commands [list $word]]]} {
                dict set opts -type "proc"
            } elseif {[llength [::tkcon::EvalOther $app $type info vars [list $word]]]} {
                dict set opts -type "var"
            } elseif {[::tkcon::EvalOther $app $type file isfile [list $word]]} {
                dict set opts -type "file"
            }
        }
        if {[dict get $opts -type] == {}} {
            return -code error "unrecognized type '$word'"
        }

        set tail $args

        return [list $word $opts $tail]
    }

    proc CompileAndLoadTheFile {clcon_text} {
        set opened_file [$clcon_text cget -opened_file]
        set FileName [$opened_file cget -filename]
        set qFileName [::tkcon::QuoteLispObjToString $FileName]
        set form "(clco::compile-file-for-tcl $qFileName t)"
        ::tkcon::FocusConsole
        ::tkcon::SendEventToSwank $form {} 1 {:find-existing}
    }

    # Note we do not save file and 
    proc FindCurrentFileDeclarations {clcon_text} {
        set opened_file [$clcon_text cget -opened_file]
        set FileName [$opened_file cget -filename]
        set qFileName [::tkcon::QuoteLispObjToString $FileName]
        set form "(clco::find-current-file-declarations $qFileName)"
        ::tkcon::SendEventToSwank $form {} 1 {:find-existing}
    }



    proc EncodeTypeForBufferList {type} {
        switch -exact $type {
            file {return "f"}
            proc {return "p"}
            var {return "v"}
            error {return "e"}
            default {return "?"}
        }
    }

    proc FindSourceContinuation {clcon_text EventAsList} {
        set Head [::mprs::Unleash [lindex $EventAsList 0]]
        ::mprs::AssertEq $Head ":return"
        set l2 [::mprs::Unleash [lindex $EventAsList 1]]
        set h2 [::mprs::Unleash [lindex $l2 0]]
        if {$h2 eq ":ok"} {
            set code [::mprs::Unleash [lindex $l2 1]]
            set proc [subst -nocommand {{w} {$code}}]
            # tk_messageBox -message $proc
            apply $proc [::tkcon::CurrentConsole]
            $clcon_text Unfreeze
        } else {
            tk_messageBox -parent $clcon_text -message "FindSource command aborted"
            $clcon_text Unfreeze
        }
          
    }

    # See also ::tkcon::LispFindDefinition
    proc FindSourceCommand {text} {
        set console [::tkcon::CurrentConsole]
        ::clcon_text::CallOduvanchikFunction $text "odu::find-source-command nil" {{
            ::edt::FindSourceContinuation $clcon_text $EventAsList
        }}
    }    
    
    proc ReadFileIntoString {word {RemoveLastNewline 0}} {
        set obj [string cat "__tkcon" [GenNamedCounter "ReadFileObj"]]
        if {$RemoveLastNewline} {
            set NoNewLineOption "-nonewline"
        } else {
            set NoNewLineOption ""
        }
        set cmd [subst -nocommands {
            set ${obj}(fid) [open {$word} r]
            set ${obj}(data) [read $NoNewLineOption \$${obj}(fid)]
            close \$${obj}(fid)
            after 1000 unset ${obj}
            return \$${obj}(data)
        }
                ]
        set line [::tkcon::EvalOther {} slave eval $cmd]
        return $line
        # if {$RemoveLastNewline && [string range $line end end] eq "\n"} {
        #     puts "oKi"
        #     return [string range $line 0 end-1]
        # } else {
        #     return $line
        # }
    }

    # This will be an option
    # If true, we allow for only one editor window at a time, joungling frames in it
    # New window to the same place where old one was
    proc SingleEditorWindow {} {
        return 1
    }

    # Returns list of two indices
    proc TextSelectionCoordinates {text} {
        $text tag nextrange sel 1.0 end
    }

    # This should go to special file?
    proc TextSetSelectionTo {text from to} {
        $text tag remove sel 1.0 end
        $text tag add sel $from $to
    }

    proc e_indent {text {extra "    "}} {
        set w $text
        set lineno [expr {int([$w index insert])}]
        set line [$w get $lineno.0 $lineno.end]
        regexp {^(\s*)} $line -> prefix
        if {[string index $line end] eq "\{"} {
            tk::TextInsert $w "\n$prefix$extra"
        } elseif {[string index $line end] eq "\}"} {
            if {[regexp {^\s+\}} $line]} {
                $w delete insert-[expr [string length $extra]+1]c insert-1c
                tk::TextInsert $w "\n[string range $prefix 0 end-[string length $extra]]"
            } else {
                tk::TextInsert $w "\n$prefix"
            }
        } else {
            tk::TextInsert $w "\n$prefix"
        }
    }

    proc EnsureToplevelWindowWithPathname {tw} {
        if {![winfo exists $tw]} {
            toplevel $tw
            wm withdraw $tw
        }
        return $tw
    }

    # Removes old editor bindtags and set new ones
    # New bindtags are DoubleMod$suffix, SingleMod$suffix, NoMod$suffix
    proc SetEditorBindtags {path suffix} {
        set s $suffix
        set result [list DoubleMod$s SingleMod$s NoMod$s]
        foreach bindtag [bindtags $path] {
            switch -glob -- $bindtag {
                DoubleMod* -
                SingleMod* -
                NoMod* {
                    # do nothing
                }
                default {
                    set result [linsert $result end $bindtag]
                }
            }
        }
        bindtags $path $result
    }

    proc cMenuBar {{SubMenuSuffix {}}} {
        string cat [cTW] ".mbar" $SubMenuSuffix
    }

    # Initialization of editor GUI, buffer-independent part.
    # Called only once when we create first editor window.
    # So if you change it, restart clcon 
    proc SetupEditorWindowCommon {tw} {
        variable ::tkcon::PRIV
        variable ::tkcon::COLOR
        variable ::tkcon::OPT

        wm protocol $tw WM_DELETE_WINDOW [list tk_messageBox -parent $tw -title "Attempt to close editor" -message "To remove editor window, close all edit buffers or just quit clcon"]

        set notebook [theNotebook]
        ttk::notebook $notebook
        pack $notebook -fill both -expand 1
        bind $notebook <<NotebookTabChanged>> "::edt::SwitchToThisTab %d"
        ttk::notebook::enableTraversal $notebook 

        set menu [menu [cMenuBar]]
        $tw configure -menu $menu

        # Make menu bar
        menu [::tkcon::MenuButton $menu "1.File" file]
        menu [::tkcon::MenuButton $menu "2.Edit" edit]
        menu [::tkcon::MenuButton $menu "3.Lisp" lisp]
        menu [::tkcon::MenuButton $menu "4.Tcl" tcl]
        menu [::tkcon::MenuButton $menu "7.Window" window]        
        menu [::tkcon::MenuButton $menu "Secret" secret]
        
    }
    
}
