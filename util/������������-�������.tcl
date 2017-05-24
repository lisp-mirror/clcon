destroy .демо-шрифтов
toplevel .демо-шрифтов
pack [text .демо-шрифтов.t -wrap none] -fill both -expand 1
set count 0
set tabwidth 0
foreach family [lsort -dictionary [font families]] {
    .демо-шрифтов.t tag configure f[incr count] -font [list $family 10]
    .демо-шрифтов.t insert end ${family}:\t {} \
            "abcабв Oо0\n" f$count
    set w [font measure [.демо-шрифтов.t cget -font] ${family}:]
    if {$w+5 > $tabwidth} {
        set tabwidth [expr {$w+5}]
        .демо-шрифтов.t configure -tabs $tabwidth
    }
}