package require Tk



wm title . "Erlang Logger"

text .log -state disabled -width 40 -height 24 -wrap none 
ttk::label .t1label -text "Temp 1: "
ttk::label .t1 -textvariable temp1
set temp1 "XXX"

grid .log -column 0 -row 0 -columnspan 4 -rowspan 4
grid .t1label -column 4 -row 0
grid .t1 -column 5 -row 0
 

set temp1 45.6

.t1label configure -background blue 
.t1label configure -background red   


proc writeToLog {msg} {
    set numlines [lindex [split [.log index "end - 1 line"] "."] 0]
    .log configure -state normal
    if {$numlines==24} {.log delete 1.0 2.0}
    if {[.log index "end-1c"]!="1.0"} {.log insert end "\n"}
    .log insert end "$msg"
    .log configure -state disabled
}


set temp1 733
