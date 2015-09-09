# sending text commands to oduvanchik


namespace eval ::text2odu {

    # Convert text index to oduvanchik coordinate system
    proc CoerceIndex {txt index} {
        if txt = end -> end
        if txt = 0.0 -> begin
        else {
            $txt index $index
        }
    }



}
