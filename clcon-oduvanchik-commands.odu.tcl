## Some oduvanchik commands and functions ported to tcl

namespace eval odu {}


# see odu::clcon-prompt-for-y-or-n.
# must exist means that use must reply anyway. This
# is always true with message box, so we ignore this param
proc ::odu::prompt_for_y_or_n {must_exist prompt help} {
    return [YesNoCancel $prompt $help [::edt::CurrentlyVisibleBuffer]]    
}

