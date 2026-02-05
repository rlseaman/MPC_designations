! MPC Designation Helper Function Tests - Fortran Implementation
! Tests the 6 helper functions with 77 test cases

program test_helpers
    use mpc_designation
    implicit none

    integer :: passed, failed
    character(len=80) :: result, parent
    character(len=10) :: frag
    logical :: bresult

    passed = 0
    failed = 0

    print '(A)', '=== MPC Designation Helper Function Tests (Fortran) ==='
    print '(A)', ''

    ! Test to_report_format
    print '(A)', '--- to_report_format ---'

    ! Numbered asteroids
    result = to_report_format('00001')
    call test_str(result, '       00001', 'Numbered asteroid 1', passed, failed)
    result = to_report_format('00433')
    call test_str(result, '       00433', 'Numbered asteroid 433', passed, failed)
    result = to_report_format('99999')
    call test_str(result, '       99999', 'Numbered asteroid 99999', passed, failed)
    result = to_report_format('A0000')
    call test_str(result, '       A0000', 'Numbered asteroid 100000', passed, failed)
    result = to_report_format('~0000')
    call test_str(result, '       ~0000', 'Numbered asteroid 620000', passed, failed)

    ! Provisional asteroids
    result = to_report_format('J95X00A')
    call test_str(result, '     J95X00A', 'Provisional 1995 XA', passed, failed)
    result = to_report_format('K24A12B')
    call test_str(result, '     K24A12B', 'Provisional 2024 AB12', passed, failed)

    ! Survey designations
    result = to_report_format('PLS2040')
    call test_str(result, '     PLS2040', 'Survey P-L', passed, failed)
    result = to_report_format('T3S3141')
    call test_str(result, '     T3S3141', 'Survey T-3', passed, failed)

    ! Numbered comets
    result = to_report_format('0001P')
    call test_str(result, '0001P       ', 'Comet 1P', passed, failed)
    result = to_report_format('0073P')
    call test_str(result, '0073P       ', 'Comet 73P', passed, failed)

    ! Numbered comets with fragments
    result = to_report_format('0073Pa')
    call test_str(result, '0073P      a', 'Comet 73P-A', passed, failed)
    result = to_report_format('0073Pb')
    call test_str(result, '0073P      b', 'Comet 73P-B', passed, failed)
    result = to_report_format('0073Paa')
    call test_str(result, '0073P     aa', 'Comet 73P-AA', passed, failed)
    result = to_report_format('0073Paz')
    call test_str(result, '0073P     az', 'Comet 73P-AZ', passed, failed)
    result = to_report_format('0073Pzz')
    call test_str(result, '0073P     zz', 'Comet 73P-ZZ', passed, failed)

    ! Provisional comets
    result = to_report_format('CJ95O010')
    call test_str(result, '    CJ95O010', 'Comet C/1995 O1', passed, failed)
    result = to_report_format('DJ93F020')
    call test_str(result, '    DJ93F020', 'Comet D/1993 F2', passed, failed)
    result = to_report_format('DJ93F02a')
    call test_str(result, '    DJ93F02a', 'Comet D/1993 F2-A', passed, failed)

    ! Test from_report_format
    print '(A)', ''
    print '(A)', '--- from_report_format ---'

    ! Numbered asteroids
    result = from_report_format('       00001')
    call test_str(result, '00001', 'Numbered asteroid 1', passed, failed)
    result = from_report_format('       00433')
    call test_str(result, '00433', 'Numbered asteroid 433', passed, failed)
    result = from_report_format('       A0000')
    call test_str(result, 'A0000', 'Numbered asteroid 100000', passed, failed)

    ! Provisional asteroids
    result = from_report_format('     J95X00A')
    call test_str(result, 'J95X00A', 'Provisional 1995 XA', passed, failed)

    ! Numbered comets
    result = from_report_format('0073P       ')
    call test_str(result, '0073P', 'Comet 73P', passed, failed)

    ! Numbered comets with fragments
    result = from_report_format('0073P      a')
    call test_str(result, '0073Pa', 'Comet 73P-A', passed, failed)
    result = from_report_format('0073P     aa')
    call test_str(result, '0073Paa', 'Comet 73P-AA', passed, failed)
    result = from_report_format('0073P     az')
    call test_str(result, '0073Paz', 'Comet 73P-AZ', passed, failed)

    ! Provisional comets
    result = from_report_format('    CJ95O010')
    call test_str(result, 'CJ95O010', 'Comet C/1995 O1', passed, failed)

    ! Test has_fragment
    print '(A)', ''
    print '(A)', '--- has_fragment ---'

    ! Unpacked with fragments
    bresult = has_fragment('73P-A')
    call test_bool(bresult, .true., 'Unpacked numbered comet with fragment', passed, failed)
    bresult = has_fragment('73P-AA')
    call test_bool(bresult, .true., 'Unpacked numbered comet with 2-letter fragment', passed, failed)
    bresult = has_fragment('D/1993 F2-A')
    call test_bool(bresult, .true., 'Unpacked provisional comet with fragment', passed, failed)
    bresult = has_fragment('P/1930 J1-AA')
    call test_bool(bresult, .true., 'Unpacked provisional comet with 2-letter fragment', passed, failed)

    ! Unpacked without fragments
    bresult = has_fragment('73P')
    call test_bool(bresult, .false., 'Unpacked numbered comet no fragment', passed, failed)
    bresult = has_fragment('C/1995 O1')
    call test_bool(bresult, .false., 'Unpacked provisional comet no fragment', passed, failed)

    ! Packed with fragments
    bresult = has_fragment('0073Pa')
    call test_bool(bresult, .true., 'Packed numbered comet with fragment', passed, failed)
    bresult = has_fragment('0073Paa')
    call test_bool(bresult, .true., 'Packed numbered comet with 2-letter fragment', passed, failed)
    bresult = has_fragment('DJ93F02a')
    call test_bool(bresult, .true., 'Packed provisional comet with fragment', passed, failed)

    ! Packed without fragments
    bresult = has_fragment('0073P')
    call test_bool(bresult, .false., 'Packed numbered comet no fragment', passed, failed)
    bresult = has_fragment('CJ95O010')
    call test_bool(bresult, .false., 'Packed provisional comet no fragment', passed, failed)

    ! Non-comets
    bresult = has_fragment('1995 XA')
    call test_bool(bresult, .false., 'Asteroid no fragment', passed, failed)
    bresult = has_fragment('00001')
    call test_bool(bresult, .false., 'Numbered asteroid', passed, failed)

    ! Test get_fragment
    print '(A)', ''
    print '(A)', '--- get_fragment ---'

    ! Unpacked with fragments
    frag = get_fragment('73P-A')
    call test_str(frag, 'A', 'Unpacked single fragment', passed, failed)
    frag = get_fragment('73P-AA')
    call test_str(frag, 'AA', 'Unpacked 2-letter fragment', passed, failed)
    frag = get_fragment('73P-I')
    call test_str(frag, 'I', 'Unpacked fragment I', passed, failed)
    frag = get_fragment('D/1993 F2-B')
    call test_str(frag, 'B', 'Unpacked provisional fragment', passed, failed)
    frag = get_fragment('P/1930 J1-AZ')
    call test_str(frag, 'AZ', 'Unpacked provisional 2-letter', passed, failed)

    ! Unpacked without fragments
    frag = get_fragment('73P')
    call test_str(frag, '', 'Unpacked no fragment', passed, failed)
    frag = get_fragment('C/1995 O1')
    call test_str(frag, '', 'Unpacked provisional no fragment', passed, failed)

    ! Packed with fragments
    frag = get_fragment('0073Pa')
    call test_str(frag, 'A', 'Packed single fragment', passed, failed)
    frag = get_fragment('0073Paa')
    call test_str(frag, 'AA', 'Packed 2-letter fragment', passed, failed)
    frag = get_fragment('0073Pi')
    call test_str(frag, 'I', 'Packed fragment I', passed, failed)
    frag = get_fragment('DJ93F02b')
    call test_str(frag, 'B', 'Packed provisional fragment', passed, failed)

    ! Packed without fragments
    frag = get_fragment('0073P')
    call test_str(frag, '', 'Packed no fragment', passed, failed)
    frag = get_fragment('CJ95O010')
    call test_str(frag, '', 'Packed provisional no fragment', passed, failed)

    ! Test get_parent
    print '(A)', ''
    print '(A)', '--- get_parent ---'

    ! Unpacked with fragments
    parent = get_parent('73P-A')
    call test_str(parent, '73P', 'Unpacked single fragment', passed, failed)
    parent = get_parent('73P-AA')
    call test_str(parent, '73P', 'Unpacked 2-letter fragment', passed, failed)
    parent = get_parent('D/1993 F2-B')
    call test_str(parent, 'D/1993 F2', 'Unpacked provisional fragment', passed, failed)
    parent = get_parent('P/1930 J1-AA')
    call test_str(parent, 'P/1930 J1', 'Unpacked provisional 2-letter', passed, failed)

    ! Unpacked without fragments
    parent = get_parent('73P')
    call test_str(parent, '73P', 'Unpacked no fragment', passed, failed)
    parent = get_parent('C/1995 O1')
    call test_str(parent, 'C/1995 O1', 'Unpacked provisional no fragment', passed, failed)

    ! Packed with fragments
    parent = get_parent('0073Pa')
    call test_str(parent, '0073P', 'Packed single fragment', passed, failed)
    parent = get_parent('0073Paa')
    call test_str(parent, '0073P', 'Packed 2-letter fragment', passed, failed)

    ! Packed without fragments
    parent = get_parent('0073P')
    call test_str(parent, '0073P', 'Packed no fragment', passed, failed)

    ! Non-comets (should return as-is)
    parent = get_parent('1995 XA')
    call test_str(parent, '1995 XA', 'Asteroid', passed, failed)
    parent = get_parent('00001')
    call test_str(parent, '00001', 'Numbered asteroid', passed, failed)

    ! Test designations_equal
    print '(A)', ''
    print '(A)', '--- designations_equal ---'

    ! Same designation, different formats
    bresult = designations_equal('1995 XA', 'J95X00A')
    call test_bool(bresult, .true., 'Provisional packed/unpacked', passed, failed)
    bresult = designations_equal('73P', '0073P')
    call test_bool(bresult, .true., 'Numbered comet packed/unpacked', passed, failed)
    bresult = designations_equal('73P-A', '0073Pa')
    call test_bool(bresult, .true., 'Comet with fragment packed/unpacked', passed, failed)
    bresult = designations_equal('73P-AA', '0073Paa')
    call test_bool(bresult, .true., 'Comet with 2-letter fragment', passed, failed)
    bresult = designations_equal('1', '00001')
    call test_bool(bresult, .true., 'Numbered asteroid', passed, failed)
    bresult = designations_equal('C/1995 O1', 'CJ95O010')
    call test_bool(bresult, .true., 'Provisional comet', passed, failed)

    ! Different designations
    bresult = designations_equal('1995 XA', '1995 XB')
    call test_bool(bresult, .false., 'Different provisional', passed, failed)
    bresult = designations_equal('73P-A', '73P-B')
    call test_bool(bresult, .false., 'Different fragments', passed, failed)
    bresult = designations_equal('73P', '74P')
    call test_bool(bresult, .false., 'Different comet numbers', passed, failed)
    bresult = designations_equal('1', '2')
    call test_bool(bresult, .false., 'Different asteroid numbers', passed, failed)

    ! Same designation (both packed or both unpacked)
    bresult = designations_equal('1995 XA', '1995 XA')
    call test_bool(bresult, .true., 'Same unpacked', passed, failed)
    bresult = designations_equal('J95X00A', 'J95X00A')
    call test_bool(bresult, .true., 'Same packed', passed, failed)

    ! Summary
    print '(A)', ''
    print '(A)', '=================================================='
    print '(A,I0,A,I0,A,I0)', 'Total: ', passed + failed, ', Passed: ', passed, ', Failed: ', failed

    if (failed > 0) then
        stop 1
    end if

contains

    subroutine test_str(result, expected, desc, passed, failed)
        character(len=*), intent(in) :: result, expected, desc
        integer, intent(inout) :: passed, failed

        if (trim(result) == trim(expected)) then
            print '(A,A,A)', '  PASS: "', trim(result), '"'
            passed = passed + 1
        else
            print '(A,A,A,A,A)', '  FAIL: expected "', trim(expected), '", got "', trim(result), '"'
            failed = failed + 1
        end if
    end subroutine test_str

    subroutine test_bool(result, expected, desc, passed, failed)
        logical, intent(in) :: result, expected
        character(len=*), intent(in) :: desc
        integer, intent(inout) :: passed, failed

        if (result .eqv. expected) then
            print '(A,A,A,L1)', '  PASS: ', desc, ' -> ', result
            passed = passed + 1
        else
            print '(A,A,A,L1,A,L1)', '  FAIL: ', desc, ' -> expected ', expected, ', got ', result
            failed = failed + 1
        end if
    end subroutine test_bool

end program test_helpers
