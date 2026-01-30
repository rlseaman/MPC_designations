## test_csv.nim - CSV benchmark test for MPC Designation Converter (Nim)

import ../src/mpc_designation
import os, strutils, times

proc main() =
  if paramCount() < 1:
    echo "Usage: test_csv <csv_file>"
    quit(1)

  let csvFile = paramStr(1)
  if not fileExists(csvFile):
    echo "Error: File not found: ", csvFile
    quit(1)

  var passed = 0
  var failed = 0
  var first = true

  let startTime = cpuTime()

  for line in lines(csvFile):
    if first:
      first = false
      continue  # Skip header

    let parts = line.split(',')
    if parts.len < 2:
      continue

    let unpacked = parts[0]
    let expectedPacked = parts[1]

    let result = convertSimple(unpacked)
    if result == expectedPacked:
      inc passed
    else:
      inc failed

  let endTime = cpuTime()
  let elapsed = (endTime - startTime) * 1000  # Convert to ms
  let total = passed + failed
  let rate = if elapsed > 0: float(total) * 1000.0 / elapsed else: 0.0

  echo "Passed: ", passed
  echo "Failed: ", failed
  echo "Time: ", int(elapsed), "ms (", formatFloat(rate, ffDecimal, 1), " entries/sec)"

when isMainModule:
  main()
