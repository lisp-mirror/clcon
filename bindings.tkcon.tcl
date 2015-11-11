# Bindings for console (but not the ones from the menu)
# FIXME - rename to console_bindings.tkcon.tcl 

namespace eval tkcon {

    proc Bindings {} {
        variable PRIV
        global tcl_platform tk_version

        #-----------------------------------------------------------------------
        # Elements of tk::Priv that are used in this file:
        #
        # mouseMoved -	Non-zero means the mouse has moved a significant
        #			amount since the button went down (so, for example,
        #			start dragging out a selection).
        #-----------------------------------------------------------------------

        ## Get all Text bindings into TkConsole
        foreach ev [bind Text] { bind TkConsole $ev [bind Text $ev] }
        ## We really didn't want the newline insertion
        bind TkConsole <Control-Key-o> {}

        ## in 8.6b3, the virtual events <<NextLine>> and <<PrevLine>> 
        #  mess up our history feature
        #bind TkConsole <<NextLine>> {}
        #bind TkConsole <<PrevLine>> {}

        ## Now make all our virtual event bindings
        set bindings {
            <<TkCon_Exit>>		<$PRIV(CTRL)-q>
            <<TkCon_New>>		<$PRIV(CTRL)-N>
            <<TkCon_NewTab>>	<$PRIV(CTRL)-T>
            <<TkCon_NextTab>>	<Control-Key-Tab>
            <<TkCon_PrevTab>>	<Control-Shift-Key-Tab>
            <<TkCon_Close>>		<$PRIV(CTRL)-w>
            <<TkCon_About>>		<$PRIV(CTRL)-A>
            <<TkCon_Find>>		<$PRIV(CTRL)F>
            <<TkCon_Slave>>		<$PRIV(CTRL)Key-1>
            <<TkCon_Master>>	<$PRIV(CTRL)Key-2>
            <<TkCon_Main>>		<$PRIV(CTRL)Key-3>
            <<TkCon_ExpandFile>>	<Control-Key-F3>
            <<TkCon_ExpandTcl>>	        <Control-Alt-Key-u>
            <<TkCon_ExpandTcl>>	        <Control-Alt-Key-Cyrillic_ghe>
            <<TkCon_ExpandLisp>>	<Control-Alt-Key-i>
            <<TkCon_ExpandLisp>>	<Key-Tab>
            <<TkCon_LispFindDefinition>> <Alt-period>
            <<TkCon_LispFindDefinition>> <Alt-Key-Cyrillic_yu>
            <<TkCon_TclFindDefinition>> <Control-Key-F9>
            <<TkCon_Tab>>		<Control-i>
            <<TkCon_Tab>>		<Alt-i>
            <<TkCon_Newline>>	<Control-Key-Return>
            <<TkCon_Newline>>	<Control-Key-KP_Enter>
            <<TkCon_Eval>>		<Return>
            <<TkCon_Eval>>		<KP_Enter>
            <<TkCon_Clear>>		<Control-l>
            <<TkCon_PreviousImmediate>>	<Control-p>
            <<TkCon_PreviousSearch>>	<Control-r>
            <<TkCon_NextImmediate>>	<Control-n>
            <<TkCon_NextSearch>>	<Control-s>
            <<TkCon_Transpose>>	<Control-t>
            <<TkCon_ClearLine>>	<Control-u>
        }
        if {$PRIV(AQUA)} {
            lappend bindings <<TkCon_Popup>> <Control-Button-1> \
                <<TkCon_Popup>> <Button-2>
        } else {
            lappend bindings <<TkCon_Popup>> <Button-3>
        }
        foreach {ev key} [subst -nocommand -noback $bindings] {
            event add $ev $key
            ## Make sure the specific key won't be defined
            bind TkConsole $key {}
        }

        # The same for TkConsoleTextOverrides
        set bindings2 {
            <<PrevLine>> <Key-Up>
            <<NextLine>> <Key-Down>
            <<TkCon_Previous>> <Control-Key-Up>
            <<TkCon_Next>> <Control-Key-Down>
        }
        foreach {ev key} [subst -nocommand -noback $bindings2] {
            event add $ev $key
            ## Make sure the specific key won't be defined
            bind TkConsoleTextOverrides $key {}
        }
        
        ## Make the ROOT bindings
        bind $PRIV(root) <<TkCon_Exit>>	exit
        bind $PRIV(root) <<TkCon_New>>	{ ::tkcon::New }
        bind $PRIV(root) <<TkCon_NewTab>>	{ ::tkcon::NewTab }
        bind $PRIV(root) <<TkCon_NextTab>>	{ ::tkcon::GotoTab 1 ; break }
        bind $PRIV(root) <<TkCon_PrevTab>>	{ ::tkcon::GotoTab -1 ; break }
        bind $PRIV(root) <<TkCon_Close>>	{ ::tkcon::Destroy }
        bind $PRIV(root) <<TkCon_About>>	{ ::tkcon::About }

        set cmd { ::fndrpl::OpenFindBox $::tkcon::PRIV(console) "text" "find" {}}
        bind $PRIV(root) <<TkCon_Find>>	$cmd
        bind $PRIV(root) <Control-Key-f>	$cmd
        bind $PRIV(root) <Control-Key-Cyrillic_a> $cmd
        
        bind $PRIV(root) <<TkCon_Slave>>	{
            ::tkcon::Attach {}
            ::tkcon::RePrompt "\n" [::tkcon::CmdGet $::tkcon::PRIV(console)]
        }
        bind $PRIV(root) <<TkCon_Master>>	{
            if {[string compare {} $::tkcon::PRIV(name)]} {
                ::tkcon::Attach $::tkcon::PRIV(name)
            } else {
                ::tkcon::Attach Main
            }
            ::tkcon::RePrompt "\n" [::tkcon::CmdGet $::tkcon::PRIV(console)]
        }
        bind $PRIV(root) <<TkCon_Main>>	{
            ::tkcon::Attach Main
            ::tkcon::RePrompt "\n" [::tkcon::CmdGet $::tkcon::PRIV(console)]
        }
        bind $PRIV(root) <<TkCon_Popup>> {
            ::tkcon::PopupMenu %X %Y
        }

        ## Menu items need null TkConsolePost bindings to avoid the TagProc
        ##
        foreach ev [bind $PRIV(root)] {
            bind TkConsolePost $ev {
                # empty
            }
        }


        # ::tkcon::ClipboardKeysyms --
        # This procedure is invoked to identify the keys that correspond to
        # the copy, cut, and paste functions for the clipboard.
        #
        # Arguments:
        # copy -	Name of the key (keysym name plus modifiers, if any,
        #		such as "Meta-y") used for the copy operation.
        # cut -		Name of the key used for the cut operation.
        # paste -	Name of the key used for the paste operation.

        proc ::tkcon::ClipboardKeysyms {copy cut paste} {
            bind TkConsole <$copy>	{::tkcon::Copy %W}
            bind TkConsole <$cut>	{::tkcon::Cut %W}
            bind TkConsole <$paste>	{::tkcon::Paste %W}
        }

        proc ::tkcon::GetSelection {w} {
            if {
                ![catch {selection get -displayof $w -type UTF8_STRING} txt] ||
                ![catch {selection get -displayof $w} txt] ||
                ![catch {selection get -displayof $w -selection CLIPBOARD} txt]
            } {
                return $txt
            }
            return -code error "could not find default selection"
        }

        proc ::tkcon::Cut w {
            if {[string match $w [selection own -displayof $w]]} {
                clipboard clear -displayof $w
                catch {
                    set txt [selection get -displayof $w]
                    clipboard append -displayof $w $txt
                    if {[$w compare sel.first >= limit]} {
                        $w delete sel.first sel.last
                    }
                }
            }
        }
        proc ::tkcon::Copy w {
            if {[string match $w [selection own -displayof $w]]} {
                clipboard clear -displayof $w
                catch {
                    set txt [selection get -displayof $w]
                    clipboard append -displayof $w $txt
                }
            }
        }
        proc ::tkcon::Paste w {
            if {![catch {GetSelection $w} txt]} {
                catch {
                    if {[$w compare sel.first >= limit]} {
                        $w delete sel.first sel.last
                    }
                }
                if {[$w compare insert < limit]} { $w mark set insert end }
                $w insert insert $txt
                $w see insert
                if {[string match *\n* $txt]} { ::tkcon::Eval $w }
            }
        }

        ## Redefine for TkConsole what we need
        ##
        event delete <<Paste>> <$PRIV(CTRL)V>
        ::tkcon::ClipboardKeysyms <Copy> <Cut> <Paste>

        bind TkConsole <Insert> {
            catch { ::tkcon::Insert %W [::tkcon::GetSelection %W] }
        }

        bind TkConsole <Triple-1> {+
            catch {
                eval %W tag remove sel [%W tag nextrange prompt sel.first sel.last]
                eval %W tag remove sel sel.last-1c
                %W mark set insert sel.first
            }
        }

        ## binding editor needed
        ## binding <events> for .tkconrc

        bind TkConsole <<TkCon_ExpandFile>> {
            if {[%W compare insert > limit]} {::tkcon::Expand %W path}
            break ; # could check "%K" == "Tab"
        }
        bind TkConsole <<TkCon_ExpandProc>> {
            if {[%W compare insert > limit]} {::tkcon::Expand %W proc}
            break ; # could check "%K" == "Tab"
        }
        bind TkConsole <<TkCon_ExpandLisp>> {
            if {[%W compare insert > limit]} {::tkcon::Expand %W lispsymbol}
            break ; # could check "%K" == "Tab"
        }
        bind TkConsole <<TkCon_LispFindDefinition>> {
            ::tkcon::LispFindDefinition %W
            break ; # could check "%K" == "Tab"
        }
        bind TkConsole <<TkCon_TclFindDefinition>> {
            ::tkcon::TclFindDefinition %W
            break ; # could check "%K" == "Tab"
        }
        bind TkConsole <<TkCon_ExpandVar>> {
            if {[%W compare insert > limit]} {::tkcon::Expand %W var}
            break ; # could check "%K" == "Tab"
        }
        bind TkConsole <<TkCon_ExpandTcl>> {
            if {[%W compare insert > limit]} {::tkcon::Expand %W}
            break ; # could check "%K" == "Tab"
        }
        bind TkConsole <<TkCon_Tab>> {
            if {[%W compare insert >= limit]} {
                ::tkcon::Insert %W \t
            }
        }
        bind TkConsole <<TkCon_Newline>> {
            if {[%W compare insert >= limit]} {
                ::tkcon::Insert %W \n
            }
        }
        bind TkConsole <<TkCon_Eval>> {
            ::tkcon::Eval %W
        }
        bind TkConsole <Delete> {
            if {[llength [%W tag nextrange sel 1.0 end]] \
                    && [%W compare sel.first >= limit]} {
                %W delete sel.first sel.last
            } elseif {[%W compare insert >= limit]} {
                %W delete insert
                %W see insert
            }
        }
        bind TkConsole <BackSpace> {
            if {[llength [%W tag nextrange sel 1.0 end]] \
                    && [%W compare sel.first >= limit]} {
                %W delete sel.first sel.last
            } elseif {[%W compare insert != 1.0] && [%W compare insert > limit]} {
                %W delete insert-1c
                %W see insert
            }
        }
        bind TkConsole <Control-h> [bind TkConsole <BackSpace>]

        bind TkConsole <KeyPress> {
            ::tkcon::Insert %W %A
        }

        bind TkConsole <Control-a> {
            if {[%W compare {limit linestart} == {insert linestart}]} {
                tk::TextSetCursor %W limit
            } else {
                tk::TextSetCursor %W {insert linestart}
            }
        }
        bind TkConsole <Key-Home> [bind TkConsole <Control-a>]
        bind TkConsole <Control-d> {
            if {[%W compare insert < limit]} break
            %W delete insert
        }
        bind TkConsole <Control-k> {
            if {[%W compare insert < limit]} break
            if {[%W compare insert == {insert lineend}]} {
                %W delete insert
            } else {
                %W delete insert {insert lineend}
            }
        }
        bind TkConsole <<TkCon_Clear>> {
            ## Clear console buffer, without losing current command line input
            set ::tkcon::PRIV(tmp) [::tkcon::CmdGet %W]
            clear
            ::tkcon::Prompt {} $::tkcon::PRIV(tmp)
        }
        bind TkConsoleTextOverrides <<TkCon_Previous>> {
            #if {[%W compare {insert linestart} != {limit linestart}]} {
            #    tk::TextSetCursor %W [tk::TextUpDownLine %W -1]
            #} else {
            #    ::tkcon::Event -1
            #}
            ::tkcon::Event -1
            break
        }
        bind TkConsoleTextOverrides <<TkCon_Next>> {
            #if {[%W compare {insert linestart} != {end-1c linestart}]} {
            #    tk::TextSetCursor %W [tk::TextUpDownLine %W 1]
            #} else {
            #    ::tkcon::Event 1
            #}
            ::tkcon::Event 1
            break
        }
        bind TkConsole <<TkCon_NextImmediate>>  { ::tkcon::Event 1 }
        bind TkConsole <<TkCon_PreviousImmediate>> { ::tkcon::Event -1 }
        bind TkConsole <<TkCon_PreviousSearch>> {
            ::tkcon::Event -1 [::tkcon::CmdGet %W]
        }
        bind TkConsole <<TkCon_NextSearch>>	    {
            ::tkcon::Event 1 [::tkcon::CmdGet %W]
        }
        bind TkConsole <<TkCon_Transpose>>	{
            ## Transpose current and previous chars
            if {[%W compare insert > "limit+1c"]} { tk::TextTranspose %W }
        }
        bind TkConsole <<TkCon_ClearLine>> {
            ## Clear command line (Unix shell staple)
            %W delete limit end
        }
        bind TkConsole <<TkCon_SaveCommand>> {
            ## Save command buffer (swaps with current command)
            set ::tkcon::PRIV(tmp) $::tkcon::PRIV(cmdsave)
            set ::tkcon::PRIV(cmdsave) [::tkcon::CmdGet %W]
            if {[string match {} $::tkcon::PRIV(cmdsave)]} {
                set ::tkcon::PRIV(cmdsave) $::tkcon::PRIV(tmp)
            } else {
                %W delete limit end-1c
            }
            ::tkcon::Insert %W $::tkcon::PRIV(tmp)
            %W see end
        }
        catch {bind TkConsole <Key-Page_Up>   { tk::TextScrollPages %W -1 }}
        catch {bind TkConsole <Key-Prior>     { tk::TextScrollPages %W -1 }}
        catch {bind TkConsole <Key-Page_Down> { tk::TextScrollPages %W 1 }}
        catch {bind TkConsole <Key-Next>      { tk::TextScrollPages %W 1 }}
        bind TkConsole <Alt-d> {
            if {[%W compare insert >= limit]} {
                %W delete insert {insert wordend}
            }
        }
        bind TkConsole <Alt-BackSpace> {
            if {[%W compare {insert -1c wordstart} >= limit]} {
                %W delete {insert -1c wordstart} insert
            }
        }
        bind TkConsole <Alt-Delete> {
            if {[%W compare insert >= limit]} {
                %W delete insert {insert wordend}
            }
        }
        bind TkConsole <ButtonRelease-2> {
            if {
                (!$tk::Priv(mouseMoved) || $tk_strictMotif) &&
                ![catch {::tkcon::GetSelection %W} ::tkcon::PRIV(tmp)]
            } {
                if {[%W compare @%x,%y < limit]} {
                    %W insert end $::tkcon::PRIV(tmp)
                } else {
                    %W insert @%x,%y $::tkcon::PRIV(tmp)
                }
                if {[string match *\n* $::tkcon::PRIV(tmp)]} {::tkcon::Eval %W}
            }
        }

        ##
        ## End TkConsole bindings
        ##

        ##
        ## Bindings for doing special things based on certain keys
        ##
        bind TkConsolePost <Key-parenright> {
            if {$::tkcon::OPT(lightbrace) && $::tkcon::OPT(blinktime)>99 && \
                    [string compare \\ [%W get insert-2c]]} {
                ::tkcon::MatchPair %W \( \) limit
            }
            set ::tkcon::PRIV(StatusCursor) [%W index insert]
        }
        bind TkConsolePost <Key-bracketright> {
            if {$::tkcon::OPT(lightbrace) && $::tkcon::OPT(blinktime)>99 && \
                    [string compare \\ [%W get insert-2c]]} {
                ::tkcon::MatchPair %W \[ \] limit
            }
            set ::tkcon::PRIV(StatusCursor) [%W index insert]
        }
        bind TkConsolePost <Key-braceright> {
            if {$::tkcon::OPT(lightbrace) && $::tkcon::OPT(blinktime)>99 && \
                    [string compare \\ [%W get insert-2c]]} {
                ::tkcon::MatchPair %W \{ \} limit
            }
            set ::tkcon::PRIV(StatusCursor) [%W index insert]
        }
        bind TkConsolePost <Key-quotedbl> {
            if {$::tkcon::OPT(lightbrace) && $::tkcon::OPT(blinktime)>99 && \
                    [string compare \\ [%W get insert-2c]]} {
                ::tkcon::MatchQuote %W limit
            }
            set ::tkcon::PRIV(StatusCursor) [%W index insert]
        }

        bind TkConsolePost <KeyPress> {
            if {[winfo exists "%W"]} {
                if {$::tkcon::OPT(lightcmd) && [string compare {} %A]} {
                    ::tkcon::TagProc %W
                }
                set ::tkcon::PRIV(StatusCursor) [%W index insert]
            }
        }

        bind TkConsolePost <Button-1> {
            set ::tkcon::PRIV(StatusCursor) [%W index insert]
        }
        bind TkConsolePost <B1-Motion> {
            set ::tkcon::PRIV(StatusCursor) [%W index insert]
        }

    }
}
