! Test MPC designation conversion with bidirectional timing and round-trip verification.
!
! Tests:
! 1. Pack direction (unpacked -> packed) with timing
! 2. Unpack direction (packed -> unpacked) with timing
! 3. Unpacked round-trip: unpack(pack(x)) = x
! 4. Packed round-trip: pack(unpack(y)) = y

program test_roundtrip
    use mpc_designation
    implicit none

    character(len=256) :: csv_file, line
    character(len=80) :: unpacked, packed, expected, got, repacked
    character(len=80), allocatable :: unpacked_list(:), packed_list(:)
    integer :: unit_num, ios, total, i, idx
    integer :: pack_passed, pack_failed, unpack_passed, unpack_failed
    integer :: rt_unpacked_passed, rt_unpacked_failed
    integer :: rt_packed_passed, rt_packed_failed
    real(8) :: start_time, end_time, pack_time, unpack_time
    real(8) :: rt_unpacked_time, rt_packed_time, rate
    integer :: count_start, count_end, count_rate
    character(len=80), allocatable :: temp_unpacked(:), temp_packed(:)

    ! Get command line argument
    if (command_argument_count() < 1) then
        print '(A)', 'Usage: test_roundtrip <csv_file>'
        stop 1
    end if

    call get_command_argument(1, csv_file)

    ! Count lines first
    unit_num = 10
    open(unit=unit_num, file=csv_file, status='old', iostat=ios)
    if (ios /= 0) then
        print '(A)', 'Error opening file: ' // trim(csv_file)
        stop 1
    end if

    total = 0
    read(unit_num, '(A)', iostat=ios) line  ! Skip header
    do
        read(unit_num, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) > 0) total = total + 1
    end do
    close(unit_num)

    ! Allocate arrays
    allocate(unpacked_list(total))
    allocate(packed_list(total))

    ! Read data
    open(unit=unit_num, file=csv_file, status='old')
    read(unit_num, '(A)') line  ! Skip header

    idx = 0
    do
        read(unit_num, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle

        idx = idx + 1
        i = index(line, ',')
        if (i > 0) then
            unpacked_list(idx) = line(1:i-1)
            packed_list(idx) = line(i+1:)
            ! Remove any trailing commas or extra content
            i = index(packed_list(idx), ',')
            if (i > 0) packed_list(idx) = packed_list(idx)(1:i-1)
        end if
    end do
    close(unit_num)

    total = idx
    print '(A,I0,A)', 'Loaded ', total, ' test cases'
    print '(A)', ''

    ! ========== Phase 1: Pack (unpacked -> packed) ==========
    print '(A)', '=== Phase 1: Pack (unpacked -> packed) ==='
    pack_passed = 0
    pack_failed = 0

    call system_clock(count_start, count_rate)

    do i = 1, total
        call clear_error()
        got = convert_simple(trim(unpacked_list(i)))
        if (MPCDesignationError) then
            pack_failed = pack_failed + 1
        else if (trim(got) == trim(packed_list(i))) then
            pack_passed = pack_passed + 1
        else
            pack_failed = pack_failed + 1
        end if
    end do

    call system_clock(count_end)
    pack_time = real(count_end - count_start, 8) / real(count_rate, 8) * 1000.0d0
    rate = real(total, 8) / (pack_time / 1000.0d0)

    print '(A,I0)', 'Passed: ', pack_passed
    print '(A,I0)', 'Failed: ', pack_failed
    print '(A,I0,A,F12.1,A)', 'Time:   ', nint(pack_time), 'ms (', rate, ' entries/sec)'
    print '(A)', ''

    ! ========== Phase 2: Unpack (packed -> unpacked) ==========
    print '(A)', '=== Phase 2: Unpack (packed -> unpacked) ==='
    unpack_passed = 0
    unpack_failed = 0

    call system_clock(count_start, count_rate)

    do i = 1, total
        call clear_error()
        got = convert_simple(trim(packed_list(i)))
        if (MPCDesignationError) then
            unpack_failed = unpack_failed + 1
        else if (trim(got) == trim(unpacked_list(i))) then
            unpack_passed = unpack_passed + 1
        else
            unpack_failed = unpack_failed + 1
        end if
    end do

    call system_clock(count_end)
    unpack_time = real(count_end - count_start, 8) / real(count_rate, 8) * 1000.0d0
    rate = real(total, 8) / (unpack_time / 1000.0d0)

    print '(A,I0)', 'Passed: ', unpack_passed
    print '(A,I0)', 'Failed: ', unpack_failed
    print '(A,I0,A,F12.1,A)', 'Time:   ', nint(unpack_time), 'ms (', rate, ' entries/sec)'
    print '(A)', ''

    ! ========== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ==========
    print '(A)', '=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ==='
    rt_unpacked_passed = 0
    rt_unpacked_failed = 0

    call system_clock(count_start, count_rate)

    do i = 1, total
        call clear_error()
        packed = convert_simple(trim(unpacked_list(i)))
        if (MPCDesignationError) then
            rt_unpacked_failed = rt_unpacked_failed + 1
            cycle
        end if
        call clear_error()
        got = convert_simple(trim(packed))
        if (MPCDesignationError) then
            rt_unpacked_failed = rt_unpacked_failed + 1
        else if (trim(got) == trim(unpacked_list(i))) then
            rt_unpacked_passed = rt_unpacked_passed + 1
        else
            rt_unpacked_failed = rt_unpacked_failed + 1
        end if
    end do

    call system_clock(count_end)
    rt_unpacked_time = real(count_end - count_start, 8) / real(count_rate, 8) * 1000.0d0
    rate = real(total, 8) / (rt_unpacked_time / 1000.0d0)

    print '(A,I0)', 'Passed: ', rt_unpacked_passed
    print '(A,I0)', 'Failed: ', rt_unpacked_failed
    print '(A,I0,A,F12.1,A)', 'Time:   ', nint(rt_unpacked_time), 'ms (', rate, ' entries/sec)'
    print '(A)', ''

    ! ========== Phase 4: Packed round-trip: pack(unpack(y)) = y ==========
    print '(A)', '=== Phase 4: Packed round-trip: pack(unpack(y)) = y ==='
    rt_packed_passed = 0
    rt_packed_failed = 0

    call system_clock(count_start, count_rate)

    do i = 1, total
        call clear_error()
        unpacked = convert_simple(trim(packed_list(i)))
        if (MPCDesignationError) then
            rt_packed_failed = rt_packed_failed + 1
            cycle
        end if
        call clear_error()
        repacked = convert_simple(trim(unpacked))
        if (MPCDesignationError) then
            rt_packed_failed = rt_packed_failed + 1
        else if (trim(repacked) == trim(packed_list(i))) then
            rt_packed_passed = rt_packed_passed + 1
        else
            rt_packed_failed = rt_packed_failed + 1
        end if
    end do

    call system_clock(count_end)
    rt_packed_time = real(count_end - count_start, 8) / real(count_rate, 8) * 1000.0d0
    rate = real(total, 8) / (rt_packed_time / 1000.0d0)

    print '(A,I0)', 'Passed: ', rt_packed_passed
    print '(A,I0)', 'Failed: ', rt_packed_failed
    print '(A,I0,A,F12.1,A)', 'Time:   ', nint(rt_packed_time), 'ms (', rate, ' entries/sec)'
    print '(A)', ''

    ! ========== Summary ==========
    print '(A)', '=== Summary ==='
    if (pack_failed == 0) then
        print '(A)', 'Pack:       PASS'
    else
        print '(A,I0,A)', 'Pack:       FAIL (', pack_failed, ')'
    end if
    if (unpack_failed == 0) then
        print '(A)', 'Unpack:     PASS'
    else
        print '(A,I0,A)', 'Unpack:     FAIL (', unpack_failed, ')'
    end if
    if (rt_unpacked_failed == 0) then
        print '(A)', 'Unpacked RT: PASS'
    else
        print '(A,I0,A)', 'Unpacked RT: FAIL (', rt_unpacked_failed, ')'
    end if
    if (rt_packed_failed == 0) then
        print '(A)', 'Packed RT:   PASS'
    else
        print '(A,I0,A)', 'Packed RT:   FAIL (', rt_packed_failed, ')'
    end if

    deallocate(unpacked_list)
    deallocate(packed_list)

    ! Exit with error only if pack or packed RT failed
    if (pack_failed > 0 .or. rt_packed_failed > 0) stop 1

end program test_roundtrip
