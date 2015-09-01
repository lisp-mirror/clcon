# Code borrowed from SLIMV
# License:      This file is placed in the public domain.
#               No warranty, express or implied.

namespace eval ::slimv {
    # Stolen from SLIMV
    variable debug               0                  # Print verbose debugging messages
    variable debug_active        0                  # Swank debugger is active  
    variable debug_activated     0                  # Swank debugger was activated
    variable current_thread      0

    variable Ssldb_level         0                  # s:sldb_level - vim variable
    variable frame_locals        [dict create]      # Map frame variable names to their index
    variable actions             [dict create]      # (Unused) swank actions (like ':write-string'), by message id
}
