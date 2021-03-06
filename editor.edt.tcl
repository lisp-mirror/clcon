## edit - opens a file/proc/var for reading/editing
## 
# Arguments:
#   edit ?option value...? -- word
#   word is file/proc/var name
#   options are:
#   -find searchstring
#   -type proc or var or file
#   -wrap wrap option for text widget
#   -offset index to show
#   type	proc/file/var (what about error?)
#   options 
# Returns:	nothing
# Added by budden : -offset option, accepts index
##

namespace eval ::edt {

    proc ShowSomeEditor {} {
        ::gui_util::FocusWindowByName [theTW] [c_text]
        return 
    }

    proc AnyBufferBi {} {
        variable EditorMRUWinList
        set p [lindex $EditorMRUWinList 0]
        if {$p eq {}} {
            return {}
        } else {
            return [dict get $p "Bi"]
        }
    }

    # Returns 1 if we must continue closing, 0 if we must keep buffer
    proc SaveDontSaveOrCancel {Bi} {
        set w [Bi2W $Bi]
        set clcon_text [Bi2btext $Bi]
        set textt [Bi2_text $Bi]
        set modified [$clcon_text edit modified]
        set MRUWinListEntry [lindex [SearchBiInMRUWinList $Bi] 1]
        set tab_name [dict get $MRUWinListEntry name]
        if {$modified} {
            if {[FileLessBufferP $clcon_text]} {
                set word "buffer"
            } else {
                set word "file"
            }
            set word2 [string toupper $word 0]
            set UserReplyToSaveRequest [YesNoCancel "Закрытие $word" "$word2 $tab_name не сохранён. Сохранить?" $textt]
            switch -exact $UserReplyToSaveRequest {
                "yes" {
                    Save $Bi $clcon_text 0
                    return 1
                }
                "no" {
                # nothing to do
                    return 1
                }
                "cancel" {
                    return 0
                }
                default {
                    error "Логическая ошибка"
                }
            }
        } else {
            return 1
        }
    }

    proc MaybeAddToRecentWhenClosingOrSwitching {Bi} {
        set clcon_text [Bi2btext $Bi]
        set opened_file [$clcon_text cget -opened_file]
        set FileName [$opened_file cget -filename]
        if {![FileLessBufferP $clcon_text]} {
            ::recent::AddRecent $FileName
        }
    }
    
    # close file (without saving for now)  
    proc EditCloseFile {Bi} {
        if {![SaveDontSaveOrCancel $Bi]} {
            return
        }
        RemoveWindowFromLists $Bi
        MaybeAddToRecentWhenClosingOrSwitching $Bi
        # UpdateMRUAndBufferList {}
        set newBi [AnyBufferBi]
        if {$newBi eq {}} {
            # no more buffers - let's kill the editor window
            after idle [list destroy [theTW]]
        } else {
            SwitchToBuffer $newBi do_nothing
            set w [Bi2W $Bi]
            after idle [list destroy $w]
        }
    }

    # Wrapped for freezed text, for menu only
    proc wesppt {script args} {
        named_args $args {-add-break 0}
        ::clcon_text::WrapEventScriptForFreezedText $script -destination [uplevel 1 {string cat "$w.text"}] -add-break $(-add-break)
    }

    proc LoadContentsAndUpdateRecent {w word opts tail} {
        set type [dict get $opts -type]
        switch -glob -- $type {
            proc*	{
                $w.text insert 1.0 \
                    [::tkcon::EvalOther {} slave dump proc [list $word]]
                after idle [list ::tkcon::Highlight $w.text tcl]
            }
            var*	{
                $w.text insert 1.0 \
                    [::tkcon::EvalOther {} slave dump var [list $word]]
                after idle [list ::tkcon::Highlight $w.text tcl]
            }
            newfile - 
            file	{

                if {$type eq "file"} {
                    if {[catch {set filemtime [file mtime $word]}]} {
                        set filemtime {}
                    }
                } else {
                    set filemtime {}
                }
                
                if {$type eq "file"} {
                    [$w.text cget -opened_file] configure -filename $word -filemtime $filemtime
                } else {
                    [$w.text cget -opened_file] configure -filename {} -filemtime $filemtime
                }
                
                ::clcon_text::ConstructBackendBuffer $w.text
                
            if {[expr {$type eq "file"}] && [expr {$filemtime ne {}}]} {
                    $w.text insert 1.0 [::edt::ReadFileIntoString $word 0]
                
                    after idle [list ::tkcon::Highlight $w.text \
                                [string trimleft [file extension $word] .]]
  
                    ::recent::AddRecent $word
                }    
            }
            error*	{
                $w.text insert 1.0 [join $tail \n]
                after idle [list ::tkcon::Highlight $w.text error]
            }
            default	{
                $w.text insert 1.0 [join $tail \n]
            }
        }
        $w.text edit reset
        # puts "Load type $type"
        if {$type eq "file"} {
            if {$filemtime eq {}} {
                $w.text edit modified 1
            } else {
                $w.text edit modified 0
            }
        } elseif {$type eq "newfile"} {
            $w.text edit modified 1
        } else {
            $w.text edit modified 0
        }
        $w.text mark set insert 1.0
    }

    proc TextModified {Bi} {
        set textt [Bi2_text $Bi]
        set modified [$textt edit modified]
        set w [Bi2W $Bi]
        set notebook [theNotebook]
        set index [$notebook index $w]
        ::buli::RefreshData
        $notebook tab $index -text [CalcTabText $Bi]
    }

    proc EditorHaveModifiedWindowsP {} {
        variable ContentKeyToBufIdDict
        dict for {key Bi} $ContentKeyToBufIdDict {
	    set textt [Bi2_text $Bi]
            set modified [$textt edit modified]
            if {$modified} {
                return 1
            }
        }
        return 0
    }

    # w is a result of  bi2W - this is in correspondence with tag names
    proc FixParentsKeyBindings {w} {
	    global ::tcl_platform

        # Fix ttk::notebook's bindings which would otherwise be shadowed by btext's bindings
        # There is a nice idea to borrow event handlers just from parents. 
        # But parent's handlers include %W and we have different %W

        bind SingleMod$w <Control-Key-Tab> [list ::ttk::notebook::TLCycleTab %W 1]
        if {$tcl_platform(platform) != "windows"} {
            bind SingleMod$w <Control-Key-ISO_Left_Tab> [list ::ttk::notebook::TLCycleTab %W -1]
		}

        bind DoubleMod$w <Control-Shift-Key-Tab> [list ::ttk::notebook::TLCycleTab %W -1]

        if {$tcl_platform(platform) != "windows"} {
            bind DoubleMod$w <Control-Shift-Key-ISO_Left_Tab> [list ::ttk::notebook::TLCycleTab %W -1]
		}

        # Disabling Shift-Key-Tab as it removes cursor from text widget. 
        if {$tcl_platform(platform) != "windows"} {
            bind SingleMod$w <Shift-Key-ISO_Left_Tab> [list break]
		}
		
		bind SingleMod$w <Shift-Key-Tab> [list break]

        # Just <Tab> is defined as indent at ::edt::MakeLispModeMenu
    }
        

    proc CalcTabText {Bi} {
        set MRUWinListEntry [lindex [SearchBiInMRUWinList [cBi]] 1]
        if {$MRUWinListEntry eq {}} { 
            return
        }
        set tab_name [dict get $MRUWinListEntry name]
        if {[string length $tab_name] > 30} {
            set tab_name [string cat [string range $tab_name 0 20] "... (" $Bi ")"]
        }
        set btext [Bi2btext $Bi]
        set asterik [BooleanToAsterik [$btext edit modified]]
        string cat $asterik $tab_name
    }

    # Init buffer GUI when it is first created 
    # variable internal_cBi is set already
    # Args:
    # opts - editor options
    proc SetupEditorWindowWhenCreatedBuffer {opts} {
        variable ::tkcon::PRIV
        variable ::tkcon::COLOR
        variable ::tkcon::OPT

        set Bi [cBi]
        set tw [theTW]
        set w [cW]
        set WStatusBar [Bi2WStatusBar $Bi]
        set btext [c_btext]
        set textt [c_text]
        set notebook [theNotebook]

        frame $w
        ::clcon_text::clcon_text $btext


        frame $WStatusBar

        label $WStatusBar.modeTitle -text "язык"
        label $WStatusBar.mode -relief sunken -borderwidth 1 -anchor w -width 10 -textvariable $btext.StatusBarInfo(Mode)

        label $WStatusBar.packageTitle -text "пак"
        label $WStatusBar.package -relief sunken -borderwidth 1 -anchor w -width 34 -textvariable $btext.StatusBarInfo(Package)
        label $WStatusBar.readtableTitle -text "тч"
        label $WStatusBar.readtable -relief sunken -borderwidth 1 -anchor w -width 24 -textvariable $btext.StatusBarInfo(Readtable)
        label $WStatusBar.cursor -relief sunken -borderwidth 1 -anchor e -width 6 \
	    -textvariable $btext.StatusBarInfo(CursorPos)

        label $WStatusBar.tick_countTitle -text "⌚"
        label $WStatusBar.tick_count -relief sunken -borderwidth 1 -anchor w -width 5 -textvariable $btext.StatusBarInfo(Tick_count)


        label $WStatusBar.connectionStatus -relief sunken -borderwidth 1 -anchor e -width 9 \
            -textvariable $btext.StatusBarInfo(ConnectionStatus)

        grid $WStatusBar.modeTitle -row 0 -column 0 -sticky nws
        grid $WStatusBar.mode -row 0 -column 1 
        grid $WStatusBar.packageTitle -row 0 -column 2 -sticky nws
        grid $WStatusBar.package -row 0 -column 3 
        grid $WStatusBar.readtableTitle -row 0 -column 4 -sticky nws
        grid $WStatusBar.readtable -row 0 -column 5 
        grid $WStatusBar.cursor -row 0 -column 6 -sticky nws 
        grid $WStatusBar.tick_countTitle -row 0 -column 7 -sticky nws
        grid $WStatusBar.tick_count -row 0 -column 8
        grid $WStatusBar.connectionStatus -row 0 -column 9 -sticky news

        # $tw.text configure -send_to_lisp 1
        # ::btext::clearHighlightClasses $btext
        
        $btext configure -wrap [dict get $opts -wrap] \
            -xscrollcommand [list $w.sx set] \
            -yscrollcommand [list $w.sy set] \
            -foreground $COLOR(stdin) \
            -background $COLOR(bg) \
            -insertbackground grey \
            -font $::tkcon::OPT(font) -borderwidth 1 -highlightthickness 0 \
            -blockcursor 1  \
            -undo 1 \
            -exportselection 0
        catch {
            # 8.5+ stuff
            set tabsp [expr {$OPT(tabspace) * [font measure $OPT(font) 0]}]
            $btext configure -tabs [list $tabsp left] -tabstyle wordprocessor
        }

        $btext configure -inactiveselectbackground [$btext cget -selectbackground]
        bind $btext <<Modified>> [list ::edt::TextModified $Bi]

        scrollbar $w.sx -orient h -command [list $w.text xview]
        scrollbar $w.sy -orient v -command [list $w.text yview]

        grid $btext -row 0 -column 0 -sticky nsew
        grid $w.sy -row 0 -column 1 -sticky ns
        grid $w.sx -row 1 -column 0 -sticky ew
        grid $WStatusBar -row 2 -column 0 -columnspan 2 -sticky ew
        
        grid columnconfigure $w 0 -weight 1
        # grid columnconfigure $w 1 -weight 1
        grid rowconfigure $w 0 -weight 1

        set tab_name [CalcTabText [cBi]]
        $notebook add $w -text $tab_name

        ::clcon_text::tncm $btext
    }

    
    # Init buffer GUI when switching to it
    # variable internal_cBi is set already
    # Args:
    # opts - editor options
    # Important variables:
    #  btext - clcon_text widget (currently is a btext wrapper)
    # Bindtags:
    #  DoubleKey$w - for double modifiers. Assigned to w, btext, textt
    #  SingleMod$w - for single modifiers. Assigned to w, btext, textt
    #  NoMod$w - for keys w/o modifiers
    proc SetupEditorWindowWhenSwitchingToIt {} {
        variable ::tkcon::PRIV
        variable ::tkcon::COLOR
        variable ::tkcon::OPT

        set MRUWinListEntry [lindex [SearchBiInMRUWinList [cBi]] 1]
        set word [dict get $MRUWinListEntry path]

        set tw [theTW]
        set w [cW]
        set btext [c_btext]
        set textt [c_text]

        set word_for_title $word
        if {[string length $word] > 50} {
            set word_for_title [string cat "... " [string range $word end-48 end]]
        } 
        wm title $tw [string cat "Ред " [string range $PRIV(lisp-title) 0 2] " " $btext " - " $word]]

        foreach path [list $tw $w $btext $textt] {
            SetEditorBindtags $path $w
        }

        FixParentsKeyBindings $w

        RebuildMenu

        EnableDisableMenuItems

    }

    proc SwitchToThisTab {{action "focus"}} {
        variable internal_cBi

        set notebook [theNotebook]
        set Bi [W2Bi [$notebook select]]

        checkValidBi $Bi
        set internal_cBi $Bi

        # We might avoid this call if the new that no switch is actually done,
        ShowingBufferChanged

        after idle [list ::edt::PerformSwitchToBufferAction $action]
    }

    proc SyncCursor {text} {
        puts "Text Index Insert = [$text index insert]"
        clcon_text::CallOduvanchikFunction $text "odu::sync-cursor-command nil"
    }
    
    # When we allow for several windows, we will have to keep correspondence
    # between widgets and windows. E.g. by namespace relationship? 
    proc w2tw {w} {
        return $w
    }

    # See docs at the beginning of file
    proc edit {args} {

        set ParsedArgs [::edt::EditorParseArgs $args]

        foreach {word opts tail} $ParsedArgs break
        
        # Find old edit window if there is one
        lassign [FindOrMakeEditorWindow $word $opts $tail] Bi word
        set w [Bi2W $Bi]
        set tw [Bi2TW $Bi]
        set btext [Bi2btext $Bi]

        # This is partially async command, not sure see insert would work now
        SwitchToBuffer $Bi focus

        if {[string compare [dict get $opts -find] {}]} {
            ::fndrpl::OldTkconFind $btext [dict get $opts -find] -case 1
        }

        if {[dict exists $opts -offset]} {
            $btext mark set insert [dict get $opts -offset]
            $btext see insert
        }
        if {[dict exists $opts -byteoffset]} {
            set mark_offset [string cat "1.0+" [dict get $opts -byteoffset] "c"]
            $btext mark set insert $mark_offset
            $btext see insert
        }


        ::clcon_text::tncm $btext

        return $btext
    }


    proc OtkrytqFajjlVDrugomEHkzemplyareIVyjjti {filename} {
        set uImyaFajjla [::WindowsFileNameToUnix $filename]
        set qImyaFajjla [::tkcon::QuoteTclStringForLisp $uImyaFajjla]
        set ComandaLispu "(swank::ed-in-emacs $qImyaFajjla :first-but-not-this)"
        ::tkcon::EvalInSwankAsync $ComandaLispu {exit} t
    }

    proc oImplementation {commandNameWoPrefix} {
        set fn [string cat "odu::" $commandNameWoPrefix "-command"]
        set w [CurrentlyVisibleBuffer]
        set txt $w.text
        set cmd [list ::clcon_text::CallOduvanchikFunction $txt "$fn nil"]
        set wcmd [::clcon_text::WrapEventScriptForFreezedText $cmd -destination $txt]
        eval $wcmd
    }   

    # (SWANK::SEND-OOB-TO-EMACS (:ED (:FILENAME "/y/yar/ит/kons/kons-test.lisp" :POSITION 4912 :BYTEP T)))
    proc ProcessEdRequest {EventAsList} {
        set FileNameSpec [::mprs::Unleash [lindex $EventAsList 1]]
        ::mprs::AssertEq [::mprs::Unleash [lindex $FileNameSpec 0]] {:filename}
        set FileName {}
        set Position "1.0"
        set ByteP 0
        set i 0
        while { $i < [ llength $FileNameSpec ] } {
            set Key [::mprs::Unleash [lindex $FileNameSpec $i ]]
            incr i 
            set Value [lindex $FileNameSpec $i]
            set ValueU [::mprs::Unleash $Value]
            incr i
            if { $Key eq {:filename} } {
              set FileName $ValueU
            } elseif { $Key eq {:position} } {
              set Position $ValueU
            } elseif { ($Key eq {:bytep}) && ![::mprs::Null $Value]} { 
              set ByteP 1
            }
        }
        set options {} 
        if { $ByteP } {
           lappend options -byteoffset $Position
        } elseif { $Position ne {}} {
           lappend options -offset $Position
        }
        set ProcessEdRequest_options $options
        ::showVar ProcessEdRequest_options
        ::edt::edit -type file {*}$options -- $FileName
    }
}

