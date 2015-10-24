## Copyright (c) Denis Budyak 2015
## Test case for bug 70
## Bug description: tcl completion: when part of namespace is given, variables not shown in completion list
## Load this file with tcsoh test/regression70.tcl

namespace eval ::autotests {
    variable NormalTime 1 # Scale of a time. In a slow computer, increase it.

    proc TimeUnits {n} {
        variable NormalTime
        return $n * $NormalTime
    }

    proc regression70 {} {
        set partial_namespace "::tkco"
        variable ::tkcon::PRIV
        set con $PRIV(console)
        event generate $con <<TkCon_Clear>>
        ::clear
        ::tkcon::Prompt
        $con insert end $partial_namespace
        event generate $con <<TkCon_ExpandTcl>>
        after [TimeUnits 0.1] [list ::autotests::regression70continuation]
    }

    proc regression70continuation {} {
        variable ::tkcon::PRIV
        set con $PRIV(console)
        set text [$con get 1.0 end]
        set success [string match "*::tkcon::PRIV*" $text]
        ::mprs::AssertEq $success 1 "regression70 test failed"
        }
    
    regression70
}
	            
