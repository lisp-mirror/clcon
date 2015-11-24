## Copyright (c) Denis Budyak 2015
## Test case for demo-tour evaluation:
## 
## "First of all try evaluate something:
##     (dotimes (i 10) (print i) (sleep 0.5)) "
## Load this file with .tcsoh test/auto/demo_tour_autotest_1.tcl

namespace eval ::autotests {
    variable NormalTime 1 # Scale of a time. In a slow computer, increase it.

    proc TimeUnits {n} {
        variable NormalTime
        return $n * $NormalTime
    }

    proc demo_tour_autotest_1 {} {
        variable ::tkcon::PRIV
        set con $PRIV(console)
        event generate $con <<TkCon_Clear>>
        ::clear
        ::tkcon::Prompt
        $con insert end "(dotimes (i 3) (print i) (sleep 0.2))"
        event generate $con <<TkCon_Eval>>
        set delay [expr {round(0.2 * 3 * 1000)+[::autotests::TimeUnits 5]}]
        after $delay [list after idle [list ::autotests::demo_tour_autotest_1_c1]]
    }

    proc demo_tour_autotest_1_c1 {} {
        variable ::tkcon::PRIV
        set con $PRIV(console)
        set text [$con get 1.0 end]
        set expected "
0 
1 
2 "
        set expected_pattern [string cat "*" $expected "*"]
        set success [string match $expected_pattern $text]
        ::mprs::AssertEq $success 1 "demo_tour_autotest_1_c1 failed"
        }
    
    demo_tour_autotest_1
}
	            
