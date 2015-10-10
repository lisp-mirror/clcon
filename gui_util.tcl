package require Tk

namespace eval ::gui_util {

    # clcon_text_options is a list
    proc frame_clcon_text_and_scrollbars {frame_pathName clcon_text_options} {
        set w $frame_pathName
        frame $w
        ::clcon_text::clcon_text $w.text
        $w.text configurelist $clcon_text_options
        scrollbar $w.sx -orient h -command [list $w.text xview]
        scrollbar $w.sy -orient v -command [list $w.text yview]
        ConfigureTextFonts $w.text
        $w.text configure \
            -xscrollcommand [list $w.sx set] \
            -yscrollcommand [list $w.sy set]
        
        # now layout elements in frame
        grid $w.text - $w.sy -sticky news
        grid $w.sx - -sticky ew
        grid columnconfigure $w 0 -weight 1
        grid columnconfigure $w 1 -weight 1
        grid rowconfigure $w 0 -weight 1

        # Don't forget to layout frame itself!
        return $w
    }

    # clcon_text with vertical scrollbar. options are in the list
    proc frame_clcon_text_and_vertical_scrollbar {frame_pathName clcon_text_options} {
        set w $frame_pathName
        frame $w
        set text $w.text
        ::clcon_text::clcon_text $text -readonly 1

        ::gui_util::ConfigureTextFonts $text
        
        $text configure -yscrollcommand [list $w.sy set] 
        scrollbar $w.sy -orient v -command [list $text yview]
        
        grid $text - $w.sy -sticky news
        grid columnconfigure $w 0 -weight 1 
        grid columnconfigure $w 1 -weight 1
        grid rowconfigure $w 0 -weight 1
    }
    

    # Sets some sensible fonts for text widget
    proc ConfigureTextFonts {text} {
        variable ::tkcon::COLOR
        $text configure \
            -foreground $COLOR(stdin) \
            -background $COLOR(bg) \
            -insertbackground $COLOR(cursor) \
            -font $::tkcon::OPT(font) -borderwidth 1 -highlightthickness 0 \
            -undo 1
    }

    # 
    # proc NoteWidgetsChanged {widget suffix_list proc_body} {
    # }
    # See example in FindBox
}
