!> Test MPC designation conversions against CSV test data
program test_csv
    use mpc_designation
    implicit none

    character(len=256) :: csv_file, line
    character(len=80) :: unpacked, expected_packed, actual_packed
    integer :: unit_num, ios, total, passed, failed
    integer :: comma_pos
    real :: start_time, end_time

    if (command_argument_count() < 1) then
        print '(A)', 'Usage: test_csv <prov_unpack_to_pack.csv>'
        stop 1
    end if

    call get_command_argument(1, csv_file)

    total = 0
    passed = 0
    failed = 0

    print '(A)', '=== MPC Designation Conversion Tests (Fortran) ==='
    print '(A)', ''

    call cpu_time(start_time)

    open(newunit=unit_num, file=trim(csv_file), status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print '(A)', 'Cannot open file: ' // trim(csv_file)
        stop 1
    end if

    ! Skip header
    read(unit_num, '(A)', iostat=ios) line

    do
        read(unit_num, '(A)', iostat=ios) line
        if (ios /= 0) exit

        if (len_trim(line) == 0) cycle

        ! Parse CSV line
        comma_pos = index(line, ',')
        if (comma_pos == 0) cycle

        unpacked = line(1:comma_pos-1)
        expected_packed = trim(line(comma_pos+1:))

        total = total + 1

        call clear_error()
        actual_packed = convert_simple(unpacked)

        if (MPCDesignationError) then
            failed = failed + 1
            if (failed <= 10) then
                print '(A)', 'FAIL: pack(' // "'" // trim(unpacked) // "'" // ')'
                print '(A)', '      Expected: ' // "'" // trim(expected_packed) // "'"
                print '(A)', '      Got error: ' // trim(error_message)
            end if
        else if (trim(actual_packed) /= trim(expected_packed)) then
            failed = failed + 1
            if (failed <= 10) then
                print '(A)', 'FAIL: pack(' // "'" // trim(unpacked) // "'" // ')'
                print '(A)', '      Expected: ' // "'" // trim(expected_packed) // "'"
                print '(A)', '      Got:      ' // "'" // trim(actual_packed) // "'"
            end if
        else
            passed = passed + 1
        end if

        ! Progress indicator
        if (mod(total, 100000) == 0) then
            print '(A,I0,A)', 'Processed ', total, ' entries...'
        end if
    end do

    close(unit_num)

    call cpu_time(end_time)

    print '(A)', ''
    print '(A)', '=== Conversion Test Results ==='
    print '(A,I0)', 'Total:  ', total
    print '(A,I0)', 'Passed: ', passed
    print '(A,I0)', 'Failed: ', failed
    print '(A,F8.2,A)', 'Time:   ', (end_time - start_time), ' seconds'

    if (failed > 10) then
        print '(A)', '(Showing first 10 failures only)'
    end if

    if (failed > 0) stop 1

end program test_csv
