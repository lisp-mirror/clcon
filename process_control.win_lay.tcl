# (C) Denis Budyak 2017, include MIT License here
# Measuring of the screen size, so that we could apply a window layout to that screen
# This file contains process control utils

namespace eval ::win_lay {
    ### Variables to track the measuring event chain
    ## MeasuringProcessState: 
    ##  0 - no measuring goes on now,
    ##  1 - measuring goes on now, so attempts to start new measuring would fail
    variable MeasuringProcessState 0
    ## Period of check for Measuring success
    variable MeasuringTickMs 100
    ## Timeout and threshold time
    variable MeasuringTimeoutMs 2000
    ## what will happen if daylight saving moves a clock? 
    variable MeasuringEndDueTime 

    ## Lambda to call after measuring (ever successful or not)
    variable LambdaToCallAfterMeasuring
}

### Controlling process of waiting (should it be a "class" for re-use?)
## StartMeasuring tries to initiate timeout guard and set up process state
## Return value: 
##  0, if measuring process was already running (failed to start, continuation won't execute)
##  1, on success
proc ::win_lay::StartMeasuring {Lambda} {
    variable MeasuringProcessState
    variable MeasuringTimeoutMs
    variable MeasuringTickMs
    if {$MeasuringProcessState == 1} {
      return 0 
    } else {
      set MeasuringProcessState 1
      variable LambdaToCallAfterMeasuring $Lambda
      variable MeasuringEndDueTime [expr [clock milliseconds] + $MeasuringTimeoutMs ]
      # initiate timeout guard
      after $MeasuringTickMs {::win_lay::CheckIfMeasuringFinished}
      return 1
    }
}

## CheckIfMeasuringFinished
## Is called from any event in the chain, and also from the timeout guard
## event chain. If measuring timed out, cancels waiting and issues
## a warning. Returns 1 if measuring process finished (no matter successfully or not)
## If event in measuring chain detected that measuring is finished, it must exit w/o 
## any further activity. 
proc ::win_lay::CheckIfMeasuringFinished {} {
    variable MeasuringProcessState
    variable MeasuringTickMs 
    variable MeasuringEndDueTime
    if {$MeasuringProcessState == 0} {
      return 1
    } elseif { [clock milliseconds] > $MeasuringEndDueTime } {
      puts stderr "Screen size measuring timed out, setting default screen sizes"
      ::win_lay::SetDefaultScreenSizes
      ::win_lay::FinishMeasuring
      return 1
    } else { 
      # wait a bit and check again
      after $MeasuringTickMs {::win_lay::CheckIfMeasuringFinished}
      return 0
    }
}

proc ::win_lay::FinishMeasuring {} {
    variable LambdaToCallAfterMeasuring
    variable MeasuringProcessState 0
    catch {destroy .measureScreenBig}
    catch {destroy .measureScreenSmall}
    apply $LambdaToCallAfterMeasuring
}

