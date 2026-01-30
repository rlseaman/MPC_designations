## test_roundtrip.nim - Roundtrip benchmark test for MPC Designation Converter (Nim)

import ../src/mpc_designation
import os, strutils, times

proc main() =
  if paramCount() < 1:
    echo "Usage: test_roundtrip <csv_file>"
    quit(1)

  let csvFile = paramStr(1)
  if not fileExists(csvFile):
    echo "Error: File not found: ", csvFile
    quit(1)

  # Load all data first
  var unpackedData: seq[string]
  var packedData: seq[string]
  var first = true

  for line in lines(csvFile):
    if first:
      first = false
      continue  # Skip header

    let parts = line.split(',')
    if parts.len < 2:
      continue

    unpackedData.add(parts[0])
    packedData.add(parts[1])

  let total = unpackedData.len
  echo "Processing ", total, " entries"
  echo ""

  # Phase 1: Pack (unpacked -> packed)
  echo "=== Phase 1: Pack ==="
  var packPassed = 0
  var packFailed = 0

  let packStart = cpuTime()
  for i in 0..<total:
    let result = convertSimple(unpackedData[i])
    if result == packedData[i]:
      inc packPassed
    else:
      inc packFailed
  let packEnd = cpuTime()
  let packElapsed = (packEnd - packStart) * 1000
  let packRate = if packElapsed > 0: float(total) * 1000.0 / packElapsed else: 0.0

  echo "Passed: ", packPassed
  echo "Failed: ", packFailed
  echo "Time: ", int(packElapsed), "ms (", formatFloat(packRate, ffDecimal, 1), " entries/sec)"
  echo ""

  # Phase 2: Unpack (packed -> unpacked)
  echo "=== Phase 2: Unpack ==="
  var unpackPassed = 0
  var unpackFailed = 0

  let unpackStart = cpuTime()
  for i in 0..<total:
    let result = convertSimple(packedData[i])
    if result == unpackedData[i]:
      inc unpackPassed
    else:
      inc unpackFailed
  let unpackEnd = cpuTime()
  let unpackElapsed = (unpackEnd - unpackStart) * 1000
  let unpackRate = if unpackElapsed > 0: float(total) * 1000.0 / unpackElapsed else: 0.0

  echo "Passed: ", unpackPassed
  echo "Failed: ", unpackFailed
  echo "Time: ", int(unpackElapsed), "ms (", formatFloat(unpackRate, ffDecimal, 1), " entries/sec)"
  echo ""

  # Round-trip verification: pack(unpack(packed)) == packed
  echo "=== Round-trip Verification ==="
  var rtPassed = 0
  var rtFailed = 0

  for i in 0..<total:
    let unpacked = convertSimple(packedData[i])
    let repacked = convertSimple(unpacked)
    if repacked == packedData[i]:
      inc rtPassed
    else:
      inc rtFailed

  if rtFailed == 0:
    echo "Packed RT: PASS (", rtPassed, "/", total, ")"
  else:
    echo "Packed RT: FAIL (", rtFailed, " failures)"

when isMainModule:
  main()
