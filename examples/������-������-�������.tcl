proc fontchooserDemo {} {
    destroy .fcd
    toplevel .fcd
    wm title .fcd "Демонстрация команды tk fontchooser"
    tk fontchooser configure -parent .fcd
    button .fcd.b -command fontchooserToggle -takefocus 0
    fontchooserVisibility .fcd.b
    bind .fcd <<TkFontchooserVisibility>> \
            [list fontchooserVisibility .fcd.b]
    foreach w {.fcd.t1 .fcd.t2} {
        text $w -width 20 -height 4 -borderwidth 1 -relief solid
        bind $w <FocusIn> [list fontchooserFocus $w]
        $w insert end "Виджет 'текст' аa О0O $w"
    }
    .fcd.t1 configure -font {Courier 14}
    .fcd.t2 configure -font {Times 16}
    pack .fcd.b .fcd.t1 .fcd.t2; focus .fcd.t1
}
proc fontchooserToggle {} {
    tk fontchooser [expr {
            [tk fontchooser configure -visible] ?
            "hide" : "show"}]
}
proc fontchooserVisibility {w} {
    $w configure -text [expr {
            [tk fontchooser configure -visible] ?
            "Скрыть диалог выбора шрифтов" : "Показать диалог выбора шрифтов" }]
}
proc fontchooserFocus {w} {
    tk fontchooser configure -font [$w cget -font] \
            -command [list fontchooserFontSelection $w]
}
proc fontchooserFontSelection {w font args} {
    $w configure -font [font actual $font]
}
fontchooserDemo
