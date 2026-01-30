\ mpc_designation_cli.fs - Command-line interface for MPC Designation Converter
\
\ Usage: gforth src/mpc_designation_cli.fs <designation> [designation ...]

require ./mpc_designation.fs

: run-cli ( -- )
  argc @ 1 > if
    argc @ 1 ?do
      i arg
      2dup s" -h" s= 2 pick 2 pick s" --help" s= or if
        2drop print-usage 0 (bye)
      then
      process-arg
    loop
  else
    print-usage
  then
  0 (bye) ;

run-cli
