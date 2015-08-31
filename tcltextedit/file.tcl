#?-File
#?
#? %2File Menu%
#?
#?   %2New file.%
#?   Creates a new empty window.
#?   If you change to another window without typing in anything in the new window it dissappears.
#?
#?   %2Open.%
#?   Prompts the user for a file, and opens it in a new window.
#?
#?   %2Save.%
#?   If the file allready has a filename it will be saved at the same location and filename.
#?   If the file does not have a filename the user will be prompted for a filename. 
#?
#?   %2Save_As...%
#?   Allways prompts the user for a filename before saving the file.
#?
#?   %2Save_All.%
#?   Saves all open files, if a file is missing a filename the user will be prompted for a filename.
#?
#?   %2Recent.%
#?     See %LRecentFiles%
#?
#?   %2Net.%
#?     See %LNet%
#?
#?   %2Execute.%
#?     See %LExecute%
#?
#?   %2Close.%
#?   Closes the file in the current active window.
#?   if the file is unsaved there will be a warning message.
#?
#?   %2Close All.%
#?   Closes all open files.
#?   if there are unsaved files there will be a warning message.
#?
#?   %2Exit.%
#?   Exits the editor.
#?   if the file is unsaved there will be a warning message.
#?
#?
#? Related topics: %lFILE%


# File handling routines for TCLTextEdit
#
# The info record:
# There is one type of info records-
#  
# {file path+filename}
#
#
#External commands:
##
# Load    $info   ; Load file into next free window   
#                 ; file path+filename 
#                 ; options: -force if there is enough information in the info field the file
#                 ;           will be loaded without prompting the user
#                 ;
#                 ; if $info is incomplete the user will be propted
#
# Save $n         ; Save file from window $n
#                 ; file path+filename 
#                 ; options: -force if there is enough information in the info field the file
#                 ;           will be saved without prompting the user
#                 ;
#                 ; if $info is incomplete the user will be propted
#
# NewFile         ; Creates a new file entry in $window
#		  ; and sets the file as current
#                 ; Filename will be set to "Untitled #filenumber"
#			
# EExit           ; Checks if any open file needs to be saved (user will be prompted)
#                 ; And shuts down the Editor
#                 ; "This is probably not the right place for this routine."
#
#Internal commands:
# DoSaveFile    $info ; Save file to a filesystem
#
# DoLoadFile    $info ; Load file from filesystem
#
# GetNextFree         ; Get next free window/file number (used when creating a new window/file)
#
# Close         $n    ; Closes window/file $n If file is unsaved the user will be prompted
#
# CloseFile     $n    ; Reset all window vars for window/file $n
#
# CloseAll            ; Closes window/file $n If a file is unsaved the user will be prompted
# 
# Ask		$s    ; Presents a widget with YES/NO/CANCEL buttons, returns yes/no or cancel 
#
# FILELoadRequest $info ; File load request
# FILESaveRequest $info ; File save request
#


namespace eval file {

    namespace export Load Save GetNextFree NewFile


    #===================================== Misc formatting commands/functions  ===================================
    proc GetNextFree {} {
        global window numw

        set n -1
        set i 1

        while {$n==-1} {
            if {$window($i,echange)=="0"} {
                set n $i
            }
            incr i
            if {$i>$numw} {
                break
            }
        }

        return $n
    }


    proc NewFile {} {
        global window current_window
        c

        win::activate [GetNextFree]
        set window($current_window,change) 0
        set window($current_window,name) "Untitled #$current_window"
        set window($current_window,echange) 0
        set window($current_window,info) "file"
        .text delete 1.0 end
        .text edit reset 
    }


    #===================================== Requests ==============================
    proc FILELoadRequest {param} {
        global c
        return "file {[tk_getOpenFile -title "Open file" -initialfile [lindex $param 1]  -filetypes $c(filetypes)  ]}"
    }

    proc FILESaveRequest {param} {
        return "file {[tk_getSaveFile -title "Save file" -initialfile [lindex $param 1]]}"
    }


    #=============================== Load Level 1  =================================================

    proc DoLoadFile { fi } {
        c $fi
        if {[lindex $fi 1]!=""} {
            if {[file exist [lindex $fi 1]]!=1} { return "error File [lindex $fi 1] does not exist" }
            set f [open [lindex $fi 1]  "RDONLY" ]
            .text delete 1.0 end
            while {![eof $f]} {
                .text insert end [read $f 10000]
            }
            close $f
        }
        return $fi
    }

    #=============================== Save Level 1  =================================================
    proc DoSaveFile { fi } {
        if {[lindex $fi 1]!=""} {
            c $fi
            set f [open [lindex $fi 1] w]
            puts $f [.text get 0.1 "end-1 char"] nonewline
            close $f
        }
        return $fi
    }

    #=============================== Load Level 2  =================================================
    # With the -force argument the Load procedure tries to load the file without prompting the user,
    # however if there are any missing arguments in the info argument the user will be prompted.
    #


    proc Load {info args} {
        global current_window window

        c "Trying to load: $info"

        set inf $info

        if {$args!="-force"} {
            c "Not forced"
            switch [lindex $info 0] {
                file {set inf [FILELoadRequest $info] }
            }
        }

        win::activate [GetNextFree]

        switch [lindex $inf 0] {
            file {
                if { [llength $inf]<2 } {set inf [FILELoadRequest $inf] }
                set inf [DoLoadFile $inf]
                .text edit reset 
            }

        }

        if {[lindex $inf 1]==""} {
            c "Load was canceled!"
            global current_window
            CloseFile $current_window
            win::Prev
            return ""
        }


        if {[lindex $inf 0]=="error"} { bgerror [lrange $inf 1 end]
            return ""
        }  


        set window($current_window,name) [lindex $inf 1]
	set window($current_window,change) 0	
	set window($current_window,echange) 1
	set window($current_window,info) $inf
	addrecent $window($current_window,info)
	win::update
        c "Action taken: $inf"
        tkTextSetCursor .text 1.0
        focus .text
    }

    #=============================== Save Level 2  =================================================
    # With the -force argument the Load procedure tries to load the file without prompting the user,
    # however if there are any missing arguments in the info argument the user will be prompted.
    #


    proc Save {info args} {
        global window current_window
        set inf $info

        c "Trying to save: $info"


        if {[lindex $window($current_window,name) 0]=="Untitled"} {
            set info "file {$window($current_window,name)}"
            set args ""
        }


        if {$args!="-force"} {

            switch [lindex $info 0] {
                file {set inf [FILESaveRequest $info] }
            }
        }

        switch [lindex $inf 0] {

            file {
                if { [llength $inf]<2 } {set inf [FILESaveRequest $inf] }
                set inf [DoSaveFile $inf] 
            }

            default  {
                set inf [FILESaveRequest $inf]
                set inf [DoSaveFile $inf] 
            }

        }

        if {[lindex $inf 1]!=""} {
            set window($current_window,name) [lindex $inf 1]
            set window($current_window,change) 0	
            set window($current_window,echange) 1
            set window($current_window,info) $inf
        } else { c "Save failed" }

        c "Action taken: $inf"
        win::update
        focus .text
    }


    #=================================== Exit routines ===========================================

    proc Ask {s} {
        global r
        set ou .ou
        catch {destroy $ou}
        toplevel $ou
        wm title $ou "Save ?"
        set r "cancel"
        frame $ou.f
        label $ou.f.label -text "Text in $s has changed\n Do you wish to save it ?"
        xbutton $ou.f.yes -text "Yes"       -command "set r yes" -width 5
        xbutton $ou.f.cancel -text "Cancel" -command "set r cancel" -width 5
        xbutton $ou.f.no  -text "No"        -command "set r no" -width 5
        pack $ou.f.label
        pack $ou.f.yes $ou.f.no $ou.f.cancel -side left  -padx 10
        bind $ou <N> "set r no"
        bind $ou <n> "set r no"
        bind $ou <Y> "set r yes"
        bind $ou <y> "set r yes"
        bind $ou <C> "set r cancel"
        bind $ou <c> "set r cancel"
        bind $ou <Escape> "set r cancel"
        label $ou.img -image warn
        pack $ou.img $ou.f -side left
        powin $ou
        vwait r
        destroy $ou
        return $r
    }



    proc CloseFile {i} {
        global window 

        if {$window($i,temp)!=""} { 
            file delete $window($i,temp) -force
            set window($i,temp) ""
            #This is also done in win::activate when apropriate
	}

        set window($i,change) 0
        set window($i,name) "Untitled #$i"
        set window($i,info) ""
        set window($i,echange) 0
        .text delete 1.0 end
        .text edit reset
    }


    proc Close {i} {
        global window
        c
        if {$window($i,echange)==1} {
            if {$window($i,change)==1} {
                switch [Ask [win::pton $window($i,name)]] {
                    yes    { Save $window($i,info) -force }
                    no     {c "Nosave" }
                    cancel { return }
                }
            }
        }

        CloseFile $i
        win::Next
        win::update
    }


    proc SaveAll {} {
        global window numw current_window

        set o $current_window
        foreach n [win::names] {
            win::activate $n
            file::Save $window($n,info) -force
        }
        win::activate $o
    }


    proc CloseAll {} {
        global current_window window
        c "start"

        set savelist ""
        set nosavelist ""

        foreach i [win::names] {

            if {$window($i,echange)==1} {
                c $i

                if {$window($i,change)==1} {
                    switch [Ask [win::pton $window($i,name)]] {
                        yes     { set savelist "$savelist $i" }
                        no      { set nosavelist "$nosavelist $i"}
                        cancel  { return cancel}
                    } 
                } else {set nosavelist "$nosavelist $i"}

            }
        }

        
        #Save all files that should be saved

        foreach i $savelist {
            win::activate $i
            Save $window($i,info) -force 
            file::CloseFile $i
        }

        #And close all other files
        foreach i $nosavelist {
            win::activate $i
            file::CloseFile $i
        }


        win::update
        win::status_change 0 0 0
    }



    #eexit finds out if there are any unsaved files to save
    proc eexit {} {
        if {[CloseAll]!="cancel" } {
            c "Starting Shutdown"
            rmtempfiles
            win::exit 
            cfg::save
            c "Shutdown Complete !!"
            exit
        }
        c "No Shutdown"
    }



}