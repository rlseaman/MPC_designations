!> Test MPC designation error handling
program test_errors
    use mpc_designation
    implicit none

    character(len=256) :: csv_file, line
    character(len=80) :: category, subcategory, raw_input, expected_error, description
    character(len=80) :: input, result_output
    integer :: unit_num, ios, total, passed, failed
    integer :: p1, p2, p3, p4
    logical :: got_error
    character(len=10) :: error_type
    type(conversion_result) :: res

    if (command_argument_count() < 1) then
        print '(A)', 'Usage: test_errors <error_test_cases.csv>'
        stop 1
    end if

    call get_command_argument(1, csv_file)

    total = 0
    passed = 0
    failed = 0

    print '(A)', '=== MPC Designation Error Tests (Fortran) ==='
    print '(A)', ''

    open(newunit=unit_num, file=trim(csv_file), status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print '(A)', 'Cannot open file: ' // trim(csv_file)
        stop 1
    end if

    do
        read(unit_num, '(A)', iostat=ios) line
        if (ios /= 0) exit

        ! Skip empty lines, comments, and header
        if (len_trim(line) == 0) cycle
        if (line(1:1) == '#') cycle
        if (line(1:9) == 'category,') cycle

        ! Parse CSV line (5 fields)
        p1 = index(line, ',')
        if (p1 == 0) cycle
        category = line(1:p1-1)

        p2 = index(line(p1+1:), ',') + p1
        if (p2 == p1) cycle
        subcategory = line(p1+1:p2-1)

        p3 = index(line(p2+1:), ',') + p2
        if (p3 == p2) cycle
        raw_input = line(p2+1:p3-1)

        p4 = index(line(p3+1:), ',') + p3
        if (p4 == p3) cycle
        expected_error = line(p3+1:p4-1)

        description = line(p4+1:)

        ! Unescape input
        input = unescape_input(raw_input)
        total = total + 1

        got_error = .false.
        error_type = ''
        result_output = ''

        call clear_error()
        res = convert(input)

        if (MPCDesignationError) then
            got_error = .true.
            if (index(error_message, 'out of range') > 0 .or. &
                index(error_message, 'Invalid asteroid number') > 0) then
                error_type = 'range'
            else
                error_type = 'format'
            end if
        else
            result_output = res%output
        end if

        if (trim(expected_error) == 'valid') then
            ! Should succeed
            if (got_error) then
                failed = failed + 1
                print '(A)', 'FAIL [' // trim(category) // '/' // trim(subcategory) // ']: ' // &
                    "'" // trim(description) // "'"
                print '(A)', '      Expected: valid conversion'
                print '(A)', '      Got:      error (' // trim(error_type) // ')'
            else
                passed = passed + 1
            end if
        else
            ! Should fail
            if (.not. got_error) then
                failed = failed + 1
                print '(A)', 'FAIL [' // trim(category) // '/' // trim(subcategory) // ']: ' // &
                    "'" // trim(description) // "'"
                print '(A)', '      Expected: error (' // trim(expected_error) // ')'
                print '(A)', '      Got:      ' // "'" // trim(result_output) // "'" // ' (success)'
            else
                passed = passed + 1
            end if
        end if
    end do

    close(unit_num)

    print '(A)', ''
    print '(A)', '=== Error Test Results ==='
    print '(A,I0)', 'Total:  ', total
    print '(A,I0)', 'Passed: ', passed
    print '(A,I0)', 'Failed: ', failed

    if (failed > 0) stop 1

contains

    !> Unescape special characters in test input
    function unescape_input(s) result(r)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: r
        integer :: i, j, hex_val
        character(len=2) :: hex_str

        r = ''
        i = 1
        j = 1

        do while (i <= len_trim(s))
            if (i < len_trim(s) .and. s(i:i) == '\') then
                select case (s(i+1:i+1))
                    case ('t')
                        r(j:j) = char(9)  ! tab
                        i = i + 2
                    case ('n')
                        r(j:j) = char(10)  ! newline
                        i = i + 2
                    case ('r')
                        r(j:j) = char(13)  ! carriage return
                        i = i + 2
                    case ('f')
                        r(j:j) = char(12)  ! form feed
                        i = i + 2
                    case ('v')
                        r(j:j) = char(11)  ! vertical tab
                        i = i + 2
                    case ('x')
                        if (i + 3 <= len_trim(s)) then
                            hex_str = s(i+2:i+3)
                            read(hex_str, '(Z2)', iostat=ios) hex_val
                            if (ios == 0) then
                                r(j:j) = char(hex_val)
                                i = i + 4
                            else
                                r(j:j) = s(i:i)
                                i = i + 1
                            end if
                        else
                            r(j:j) = s(i:i)
                            i = i + 1
                        end if
                    case default
                        r(j:j) = s(i:i)
                        i = i + 1
                end select
            else
                r(j:j) = s(i:i)
                i = i + 1
            end if
            j = j + 1
        end do
    end function unescape_input

end program test_errors
