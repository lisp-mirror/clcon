########### Namespace ####################################
   
namespace eval ::clcon_key {
    variable LetterMap
    }


########### Procs ########################################

proc ::clcon_key::FillLetterMapWin32 {} {
    variable LetterMap
    set LetterMap [dict create]
    set originals [aux2]
    set Russians [aux1win32]
    set i 0
    foreach o $originals {
        set Bucket [list]
        set isLetter [expr {[string length $o] == 1}]
        if {$isLetter} {
            lappend Bucket [string toupper $o]
        }
        set Russian [lindex [lindex $Russians $i] 1]
        set CapitalRussian [string toupper $Russian 0 0]
        lappend Bucket $Russian $CapitalRussian
        dict set LetterMap $o $Bucket
        incr i
    }
    dict set LetterMap i {I oslash Ooblique}
    dict set LetterMap z {Z ydiaeresis ssharp}
}

proc ::clcon_key::aux1win32 {} {return {
    {а agrave
    } {б aacute
    } {в acircumflex
    } {г atilde
    } {д adiaeresis
    } {е aring
    } {ё cedilla
    } {ж ae
    } {з ccedilla
    } {и egrave
    } {й eacute
    } {к ecircumflex
    } {л ediaeresis
    } {м igrave
    } {н iacute
    } {о icircumflex
    } {п idiaeresis
    } {р eth
    } {с ntilde
    } {т ograve
    } {у oacute
    } {ф ocircumflex
    } {х otilde
    } {ц odiaeresis
    } {ч division
    } {ш oslash
    } {щ ugrave
    } {ъ uacute
    } {ы ucircumflex
    } {ь udiaeresis
    } {э yacute
    } {ю thorn
    } {я ydiaeresis}
}}

proc clcon_key::aux2 {} {
  return {f comma d u l t quoteleft semicolon p b q r k v y j g h c n e a bracketleft w x i o bracketright s m quoteright period z}
}


proc ::clcon_key::aux1x {} {return {
    Cyrillic_a   
    Cyrillic_be  
    Cyrillic_ve  
    Cyrillic_ghe
    Cyrillic_de
    Cyrillic_ie
    Cyrillic_io
    Cyrillic_zhe
    Cyrillic_ze
    Cyrillic_i
    Cyrillic_shorti
    Cyrillic_ka
    Cyrillic_el
    Cyrillic_em
    Cyrillic_en
    Cyrillic_o
    Cyrillic_pe
    Cyrillic_er
    Cyrillic_es
    Cyrillic_te
    Cyrillic_u
    Cyrillic_ef
    Cyrillic_ha
    Cyrillic_tse
    Cyrillic_che
    Cyrillic_sha
    Cyrillic_shcha
    Cyrillic_hardsign
    Cyrillic_yeru
    Cyrillic_softsign
    Cyrillic_e
    Cyrillic_yu
    Cyrillic_ya
    }
}


proc ::clcon_key::FillLetterMapX {} {
    variable LetterMap
    set LetterMap [dict create]
    set LetterMap [dict create]
    set originals [aux2]
    set Russians [aux1x]
    set i 0
    foreach o $originals {
        set Bucket [list]
        set isLetter [expr {[string length $o] == 1}]
        if {$isLetter} {
            lappend Bucket [string toupper $o]
        }
        set Russian [lindex $Russians $i]
        set RussianJustLetter [lindex [split $Russian _] 1]
        set CapitalRussian [string cat Cyrillic_ [string toupper $RussianJustLetter]]
        lappend Bucket $Russian $CapitalRussian
        dict set LetterMap $o $Bucket
        incr i
    }
}


proc ::clcon_key::FillLetterMap {} {
    set ws [tk windowingsystem]
    if {$ws eq "win32"} {
        FillLetterMapWin32
    } elseif {$ws eq "x11"} {
        FillLetterMapX
    }
}


proc ::clcon_key::ExtractEnglishLetter {EnglishKey} {
    set StrippedKey [string range $EnglishKey 1 end-1]
    set SplittedKey [split $StrippedKey -]
    set LastPart [lindex $SplittedKey end]
    set CutAtEnd [expr {1 + [string length $LastPart]}]
    set Prefix [string range $EnglishKey 0 "end-$CutAtEnd"]
    return [list $Prefix $LastPart]
}

proc ::clcon_key::AlternateKeys {EnglishKey} {
    variable LetterMap
    lassign [ExtractEnglishLetter $EnglishKey] EPrefix ELetter 
    set SmallELetter [string tolower $ELetter]
    if {![dict exists $LetterMap $SmallELetter]} {
        return $EnglishKey
    }
    set letters [concat $SmallELetter [dict get $LetterMap $SmallELetter]]
    set result [list]
    foreach l $letters {
        lappend result [string cat $EPrefix $l {>}]
    }
    return $result
}
        
    
# We assume key ends with some key designator
proc ::clcon_key::b {bind_bareword tag EnglishKey command} {
    ::mprs::AssertEq $bind_bareword "bind"
	set kV "<Control-Key-V>"
    foreach key [::clcon_key::AlternateKeys $EnglishKey] {
	 $bind_bareword $tag $key $command
    }
}


############## Here is the code we execute at load time ##############

::clcon_key::FillLetterMap

