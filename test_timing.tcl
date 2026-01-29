source mpc_designation.tcl

set count 0
set start [clock milliseconds]

set fp [open "prov_unpack_to_pack.csv" r]
gets $fp ;# skip header

while {[gets $fp line] >= 0} {
    set parts [split $line ,]
    set unpacked [lindex $parts 0]
    MPCDesignation::convertSimple $unpacked
    incr count
    if {$count >= 10000} break
}
close $fp

set elapsed [expr {[clock milliseconds] - $start}]
puts "Processed $count entries in ${elapsed}ms"
puts "Rate: [expr {$count * 1000.0 / $elapsed}] entries/sec"
puts "Estimated time for 2M entries: [expr {2000000 * $elapsed / $count / 1000.0}] seconds"
