!> Command-line interface for MPC designation conversion
program mpc_designation_cli
    use mpc_designation
    implicit none

    character(len=200) :: arg, designation
    type(conversion_result) :: res
    integer :: i, nargs
    logical :: verbose

    verbose = .false.
    designation = ''
    nargs = command_argument_count()

    if (nargs == 0) then
        call print_usage()
        stop 1
    end if

    i = 1
    do while (i <= nargs)
        call get_command_argument(i, arg)

        if (arg == '-h' .or. arg == '--help') then
            call print_usage()
            stop 0
        else if (arg == '--version') then
            print '(A)', 'mpc_designation ' // VERSION
            stop 0
        else if (arg == '-v' .or. arg == '--verbose') then
            verbose = .true.
        else
            ! Collect remaining args as designation
            designation = trim(arg)
            do while (i < nargs)
                i = i + 1
                call get_command_argument(i, arg)
                designation = trim(designation) // ' ' // trim(arg)
            end do
            exit
        end if

        i = i + 1
    end do

    if (len_trim(designation) == 0) then
        print '(A)', 'Error: No designation provided'
        call print_usage()
        stop 1
    end if

    res = convert(designation)

    if (MPCDesignationError) then
        print '(A)', 'Error: ' // trim(error_message)
        stop 1
    end if

    if (verbose) then
        print '(A)', 'Input:   ' // trim(res%input)
        print '(A)', 'Output:  ' // trim(res%output)
        print '(A)', 'Format:  ' // trim(res%info%format)
        print '(A)', 'Type:    ' // trim(res%info%dtype)
        print '(A)', 'Subtype: ' // trim(res%info%subtype)
    else
        print '(A)', trim(res%output)
    end if

contains

    subroutine print_usage()
        print '(A)', 'Usage: mpc_designation [options] <designation>'
        print '(A)', ''
        print '(A)', 'Convert between packed and unpacked MPC designations.'
        print '(A)', ''
        print '(A)', 'Options:'
        print '(A)', '  -v, --verbose   Show detailed format information'
        print '(A)', '  -h, --help      Show this help message'
        print '(A)', '  --version       Show version'
        print '(A)', ''
        print '(A)', 'Examples:'
        print '(A)', "  mpc_designation '1995 XA'     # Convert to packed: J95X00A"
        print '(A)', '  mpc_designation J95X00A       # Convert to unpacked: 1995 XA'
    end subroutine print_usage

end program mpc_designation_cli
