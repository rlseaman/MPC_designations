!> MPC Designation Converter Module
!>
!> Convert between packed and unpacked Minor Planet Center (MPC) designations
!> for asteroids, comets, and natural satellites.
!>
!> Based on MPC specification: https://www.minorplanetcenter.net/iau/info/PackedDes.html
module mpc_designation
    implicit none
    private

    ! Public interface
    public :: convert_simple, convert, pack_designation, unpack_designation
    public :: detect_format, is_valid_designation
    public :: pack_permanent, unpack_permanent
    public :: pack_provisional_impl, unpack_provisional
    public :: pack_comet_provisional, unpack_comet_provisional
    public :: pack_comet_numbered, unpack_comet_numbered
    public :: pack_comet_full, unpack_comet_full
    public :: pack_satellite, unpack_satellite
    public :: unpack_extended_provisional, unpack_ancient_comet_provisional
    public :: format_info, conversion_result
    public :: clear_error

    ! Constants
    character(len=*), parameter, public :: VERSION = '1.0.0'
    integer, parameter, public :: MAX_ASTEROID_NUMBER = 15396335

    ! Base-62 character set
    character(len=62), parameter :: BASE62_CHARS = &
        '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'

    ! Century codes
    character(len=12), parameter :: CENTURY_LETTERS = 'ABCDEFGHIJKL'

    ! Comet types
    character(len=6), parameter :: COMET_TYPES = 'PCDXAI'

    ! Satellite planets
    character(len=4), parameter :: SATELLITE_PLANETS = 'JSUN'

    !> Format information type
    type :: format_info
        character(len=10) :: format = ''    ! 'packed' or 'unpacked'
        character(len=25) :: dtype = ''     ! designation type
        character(len=60) :: subtype = ''   ! detailed description
    end type format_info

    !> Conversion result type
    type :: conversion_result
        character(len=80) :: input = ''
        character(len=80) :: output = ''
        type(format_info) :: info
    end type conversion_result

    !> Error flag (set by routines, must be checked by caller)
    logical, public :: MPCDesignationError = .false.
    character(len=200), public :: error_message = ''

contains

    !> Set error state
    subroutine set_error(msg)
        character(len=*), intent(in) :: msg
        MPCDesignationError = .true.
        error_message = msg
    end subroutine set_error

    !> Clear error state
    subroutine clear_error()
        MPCDesignationError = .false.
        error_message = ''
    end subroutine clear_error

    !> Trim and return string
    function trim_str(s) result(r)
        character(len=*), intent(in) :: s
        character(len=len_trim(adjustl(s))) :: r
        r = trim(adjustl(s))
    end function trim_str

    !> Check if character is a digit
    logical function is_digit(c)
        character(len=1), intent(in) :: c
        is_digit = (c >= '0' .and. c <= '9')
    end function is_digit

    !> Check if character is uppercase letter
    logical function is_upper(c)
        character(len=1), intent(in) :: c
        is_upper = (c >= 'A' .and. c <= 'Z')
    end function is_upper

    !> Check if character is lowercase letter
    logical function is_lower(c)
        character(len=1), intent(in) :: c
        is_lower = (c >= 'a' .and. c <= 'z')
    end function is_lower

    !> Check if character is valid printable ASCII (32-126)
    logical function is_valid_ascii(c)
        character(len=1), intent(in) :: c
        integer :: ic
        ic = ichar(c)
        is_valid_ascii = (ic >= 32 .and. ic <= 126)
    end function is_valid_ascii

    !> Validate input for whitespace issues and invalid characters
    subroutine validate_input(s, valid)
        character(len=*), intent(in) :: s
        logical, intent(out) :: valid
        integer :: i, slen
        character(len=1) :: c

        valid = .true.
        slen = len(s)

        ! Check for tabs and other control characters
        do i = 1, slen
            c = s(i:i)
            ! Check for tab (char 9) or other control characters
            if (ichar(c) == 9 .or. (ichar(c) < 32 .and. ichar(c) /= 0)) then
                call set_error('Invalid whitespace character in designation')
                valid = .false.
                return
            end if
            ! Check for non-ASCII characters
            if (ichar(c) > 126) then
                call set_error('Invalid character in designation')
                valid = .false.
                return
            end if
        end do

        ! Check for consecutive spaces
        do i = 1, slen - 1
            if (s(i:i) == ' ' .and. s(i+1:i+1) == ' ') then
                call set_error('Invalid whitespace: consecutive spaces')
                valid = .false.
                return
            end if
        end do

        ! Check for trailing space (after trim, nothing should remain at end)
        if (slen > 0 .and. s(slen:slen) == ' ') then
            call set_error('Invalid whitespace: trailing space')
            valid = .false.
            return
        end if
    end subroutine validate_input

    !> Check if string is all digits
    logical function is_all_digits(s)
        character(len=*), intent(in) :: s
        integer :: i
        is_all_digits = .true.
        if (len_trim(s) == 0) then
            is_all_digits = .false.
            return
        end if
        do i = 1, len_trim(s)
            if (.not. is_digit(s(i:i))) then
                is_all_digits = .false.
                return
            end if
        end do
    end function is_all_digits

    !> Check if character is valid half-month letter (A-Y excluding I)
    logical function is_valid_half_month(c)
        character(len=1), intent(in) :: c
        is_valid_half_month = (c >= 'A' .and. c <= 'Y' .and. c /= 'I')
    end function is_valid_half_month

    !> Convert base-62 character to number
    integer function base62_to_num(c)
        character(len=1), intent(in) :: c
        integer :: idx
        idx = index(BASE62_CHARS, c)
        if (idx == 0) then
            call set_error('Invalid base-62 character: ' // c)
            base62_to_num = -1
            return
        end if
        base62_to_num = idx - 1
    end function base62_to_num

    !> Convert number to base-62 character
    character(len=1) function num_to_base62(n)
        integer, intent(in) :: n
        if (n < 0 .or. n > 61) then
            call set_error('Number out of base-62 range')
            num_to_base62 = '?'
            return
        end if
        num_to_base62 = BASE62_CHARS(n+1:n+1)
    end function num_to_base62

    !> Convert base-62 string to number
    integer function base62_string_to_num(s)
        character(len=*), intent(in) :: s
        integer :: i, val
        base62_string_to_num = 0
        do i = 1, len_trim(s)
            val = base62_to_num(s(i:i))
            if (MPCDesignationError) return
            base62_string_to_num = base62_string_to_num * 62 + val
        end do
    end function base62_string_to_num

    !> Convert number to base-62 string of specified width
    function num_to_base62_string(n, width) result(s)
        integer, intent(in) :: n, width
        character(len=width) :: s
        integer :: i, num
        num = n
        do i = width, 1, -1
            s(i:i) = num_to_base62(mod(num, 62))
            if (MPCDesignationError) return
            num = num / 62
        end do
    end function num_to_base62_string

    !> Get century code for year
    integer function get_century_code(c)
        character(len=1), intent(in) :: c
        integer :: idx
        idx = index(CENTURY_LETTERS, c)
        if (idx == 0) then
            call set_error('Invalid century code: ' // c)
            get_century_code = -1
            return
        end if
        get_century_code = idx + 9  ! A=10, B=11, etc.
    end function get_century_code

    !> Get century letter for code
    character(len=1) function get_century_letter(code)
        integer, intent(in) :: code
        if (code < 10 .or. code > 21) then
            call set_error('Invalid century code')
            get_century_letter = '?'
            return
        end if
        get_century_letter = CENTURY_LETTERS(code-9:code-9)
    end function get_century_letter

    !> Decode cycle count from 2-char encoding
    integer function decode_cycle_count(encoded)
        character(len=2), intent(in) :: encoded
        integer :: tens
        character(len=1) :: first, second

        first = encoded(1:1)
        second = encoded(2:2)

        if (is_digit(first)) then
            tens = ichar(first) - ichar('0')
        else if (is_upper(first)) then
            tens = ichar(first) - ichar('A') + 10
        else if (is_lower(first)) then
            tens = ichar(first) - ichar('a') + 36
        else
            call set_error('Invalid cycle count encoding')
            decode_cycle_count = -1
            return
        end if

        if (.not. is_digit(second)) then
            call set_error('Invalid cycle count encoding')
            decode_cycle_count = -1
            return
        end if

        decode_cycle_count = tens * 10 + (ichar(second) - ichar('0'))
    end function decode_cycle_count

    !> Encode cycle count to 2-char format
    function encode_cycle_count(count) result(encoded)
        integer, intent(in) :: count
        character(len=2) :: encoded
        integer :: tens, ones

        if (count < 0 .or. count >= 620) then
            call set_error('Cycle count out of range (0-619)')
            encoded = '??'
            return
        end if

        tens = count / 10
        ones = mod(count, 10)

        if (tens < 10) then
            encoded(1:1) = char(ichar('0') + tens)
        else if (tens < 36) then
            encoded(1:1) = char(ichar('A') + tens - 10)
        else
            encoded(1:1) = char(ichar('a') + tens - 36)
        end if

        encoded(2:2) = char(ichar('0') + ones)
    end function encode_cycle_count

    !> Convert letter to position (A=1, B=2, ..., skipping I)
    integer function letter_to_position(letter)
        character(len=1), intent(in) :: letter
        letter_to_position = ichar(letter) - ichar('A') + 1
        if (letter > 'I') letter_to_position = letter_to_position - 1
    end function letter_to_position

    !> Convert position to letter (1=A, 2=B, ..., skipping I)
    character(len=1) function position_to_letter(pos)
        integer, intent(in) :: pos
        integer :: p
        if (pos < 1 .or. pos > 25) then
            call set_error('Invalid letter position')
            position_to_letter = '?'
            return
        end if
        p = pos
        if (p >= 9) p = p + 1  ! Skip I
        position_to_letter = char(ichar('A') + p - 1)
    end function position_to_letter

    !> Unpack permanent (numbered) asteroid
    integer function unpack_permanent(packed)
        character(len=*), intent(in) :: packed
        character(len=20) :: p
        character(len=1) :: first
        integer :: val, rest, ios

        call clear_error()
        p = trim_str(packed)

        if (len_trim(p) /= 5) then
            call set_error('Invalid packed permanent designation length')
            unpack_permanent = -1
            return
        end if

        first = p(1:1)

        ! Tilde format (>= 620,000)
        if (first == '~') then
            unpack_permanent = 620000 + base62_string_to_num(p(2:5))
            return
        end if

        ! Simple numeric format (< 100,000)
        if (is_digit(first)) then
            read(p, '(I5)', iostat=ios) unpack_permanent
            if (ios /= 0) then
                call set_error('Invalid packed permanent designation')
                unpack_permanent = -1
            end if
            return
        end if

        ! Extended format with uppercase letter (100,000 - 359,999)
        if (is_upper(first)) then
            val = ichar(first) - 55  ! A=10, B=11, etc.
            read(p(2:5), '(I4)', iostat=ios) rest
            if (ios /= 0) then
                call set_error('Invalid packed permanent designation')
                unpack_permanent = -1
                return
            end if
            unpack_permanent = val * 10000 + rest
            return
        end if

        ! Extended format with lowercase letter (360,000 - 619,999)
        if (is_lower(first)) then
            val = ichar(first) - 61  ! a=36, b=37, etc.
            read(p(2:5), '(I4)', iostat=ios) rest
            if (ios /= 0) then
                call set_error('Invalid packed permanent designation')
                unpack_permanent = -1
                return
            end if
            unpack_permanent = val * 10000 + rest
            return
        end if

        call set_error('Invalid packed permanent designation')
        unpack_permanent = -1
    end function unpack_permanent

    !> Pack permanent (numbered) asteroid
    function pack_permanent(number) result(packed)
        integer, intent(in) :: number
        character(len=5) :: packed
        integer :: div, imod
        character(len=1) :: letter

        call clear_error()

        if (number < 1 .or. number > MAX_ASTEROID_NUMBER) then
            call set_error('Invalid asteroid number')
            packed = '?????'
            return
        end if

        if (number < 100000) then
            write(packed, '(I5.5)') number
        else if (number < 620000) then
            div = number / 10000
            imod = mod(number, 10000)
            if (div < 36) then
                letter = char(div + 55)  ! A-Z
            else
                letter = char(div + 61)  ! a-z
            end if
            write(packed, '(A1,I4.4)') letter, imod
        else
            packed = '~' // num_to_base62_string(number - 620000, 4)
        end if
    end function pack_permanent

    !> Unpack provisional asteroid designation
    function unpack_provisional(packed) result(unpacked)
        character(len=*), intent(in) :: packed
        character(len=20) :: unpacked
        character(len=20) :: p
        character(len=1) :: century, half_month, second_letter
        character(len=2) :: year_str, order_encoded
        integer :: century_val, order_num, survey_num, ios

        call clear_error()
        p = trim_str(packed)

        ! Check for survey designations
        if (len_trim(p) == 7) then
            if (p(1:3) == 'PLS') then
                read(p(4:7), '(I4)', iostat=ios) survey_num
                if (ios == 0) then
                    write(unpacked, '(I0,A)') survey_num, ' P-L'
                    return
                end if
            else if (p(1:3) == 'T1S') then
                read(p(4:7), '(I4)', iostat=ios) survey_num
                if (ios == 0) then
                    write(unpacked, '(I0,A)') survey_num, ' T-1'
                    return
                end if
            else if (p(1:3) == 'T2S') then
                read(p(4:7), '(I4)', iostat=ios) survey_num
                if (ios == 0) then
                    write(unpacked, '(I0,A)') survey_num, ' T-2'
                    return
                end if
            else if (p(1:3) == 'T3S') then
                read(p(4:7), '(I4)', iostat=ios) survey_num
                if (ios == 0) then
                    write(unpacked, '(I0,A)') survey_num, ' T-3'
                    return
                end if
            end if
        end if

        if (len_trim(p) /= 7) then
            call set_error('Invalid packed provisional designation length')
            unpacked = ''
            return
        end if

        century = p(1:1)
        year_str = p(2:3)
        half_month = p(4:4)
        order_encoded = p(5:6)
        second_letter = p(7:7)

        century_val = get_century_code(century)
        if (MPCDesignationError) then
            unpacked = ''
            return
        end if

        order_num = decode_cycle_count(order_encoded)
        if (MPCDesignationError) then
            unpacked = ''
            return
        end if

        if (order_num == 0) then
            write(unpacked, '(I2,A2,A1,A1,A1)') century_val, year_str, ' ', half_month, second_letter
        else
            write(unpacked, '(I2,A2,A1,A1,A1,I0)') century_val, year_str, ' ', half_month, second_letter, order_num
        end if
    end function unpack_provisional

    !> Unpack extended provisional (underscore format)
    function unpack_extended_provisional(packed) result(unpacked)
        character(len=*), intent(in) :: packed
        character(len=20) :: unpacked
        character(len=20) :: p
        integer :: year_val, base_seq, cycle, letter_pos
        character(len=1) :: half_month, second_letter

        call clear_error()
        p = trim_str(packed)

        if (len_trim(p) /= 7 .or. p(1:1) /= '_') then
            call set_error('Invalid extended packed provisional')
            unpacked = ''
            return
        end if

        year_val = base62_to_num(p(2:2))
        if (MPCDesignationError) then
            unpacked = ''
            return
        end if

        half_month = p(3:3)
        base_seq = base62_string_to_num(p(4:7))
        if (MPCDesignationError) then
            unpacked = ''
            return
        end if

        cycle = 620 + base_seq / 25
        letter_pos = mod(base_seq, 25) + 1
        second_letter = position_to_letter(letter_pos)
        if (MPCDesignationError) then
            unpacked = ''
            return
        end if

        write(unpacked, '(I4,A1,A1,A1,I0)') 2000 + year_val, ' ', half_month, second_letter, cycle
    end function unpack_extended_provisional

    !> Unpack comet provisional designation
    function unpack_comet_provisional(packed) result(unpacked)
        character(len=*), intent(in) :: packed
        character(len=30) :: unpacked
        character(len=20) :: p
        character(len=1) :: century, half_month
        character(len=2) :: year_str, order_encoded, fragment
        integer :: century_val, order_num, plen

        call clear_error()
        p = trim_str(packed)
        plen = len_trim(p)

        if (plen /= 7 .and. plen /= 8) then
            call set_error('Invalid packed comet provisional designation length')
            unpacked = ''
            return
        end if

        century = p(1:1)
        year_str = p(2:3)
        half_month = p(4:4)
        order_encoded = p(5:6)

        if (plen == 7) then
            fragment = p(7:7) // ' '
        else
            fragment = p(7:8)
        end if

        century_val = get_century_code(century)
        if (MPCDesignationError) then
            unpacked = ''
            return
        end if

        order_num = decode_cycle_count(order_encoded)
        if (MPCDesignationError) then
            unpacked = ''
            return
        end if

        if (trim(fragment) == '0') then
            write(unpacked, '(I2,A2,A1,A1,I0)') century_val, year_str, ' ', half_month, order_num
        else
            write(unpacked, '(I2,A2,A1,A1,I0,A1,A)') century_val, year_str, ' ', half_month, order_num, '-', &
                to_upper(trim(fragment))
        end if
    end function unpack_comet_provisional

    !> Convert to uppercase
    function to_upper(s) result(upper)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: upper
        integer :: i, ic
        upper = s
        do i = 1, len(s)
            ic = ichar(s(i:i))
            if (ic >= ichar('a') .and. ic <= ichar('z')) then
                upper(i:i) = char(ic - 32)
            end if
        end do
    end function to_upper

    !> Convert to lowercase
    function to_lower(s) result(lower)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: lower
        integer :: i, ic
        lower = s
        do i = 1, len(s)
            ic = ichar(s(i:i))
            if (ic >= ichar('A') .and. ic <= ichar('Z')) then
                lower(i:i) = char(ic + 32)
            end if
        end do
    end function to_lower

    !> Pack comet provisional designation
    function pack_comet_provisional(unpacked) result(packed)
        character(len=*), intent(in) :: unpacked
        character(len=8) :: packed
        ! Implemented in full version
        call clear_error()
        packed = ''
        call set_error('pack_comet_provisional not fully implemented')
    end function pack_comet_provisional

    !> Unpack numbered comet
    function unpack_comet_numbered(packed) result(unpacked)
        character(len=*), intent(in) :: packed
        character(len=10) :: unpacked
        character(len=10) :: p
        integer :: num, ios
        character(len=1) :: ctype

        call clear_error()
        p = trim_str(packed)

        if (len_trim(p) /= 5) then
            call set_error('Invalid packed numbered comet designation')
            unpacked = ''
            return
        end if

        ctype = p(5:5)
        if (ctype /= 'P' .and. ctype /= 'D') then
            call set_error('Invalid packed numbered comet designation')
            unpacked = ''
            return
        end if

        read(p(1:4), '(I4)', iostat=ios) num
        if (ios /= 0) then
            call set_error('Invalid packed numbered comet designation')
            unpacked = ''
            return
        end if

        write(unpacked, '(I0,A1)') num, ctype
    end function unpack_comet_numbered

    !> Pack numbered comet
    function pack_comet_numbered(unpacked) result(packed)
        character(len=*), intent(in) :: unpacked
        character(len=5) :: packed
        character(len=20) :: u
        integer :: i, num, ios
        character(len=1) :: ctype

        call clear_error()
        u = trim_str(unpacked)

        ! Find the P or D
        do i = 1, len_trim(u)
            if (u(i:i) == 'P' .or. u(i:i) == 'D') then
                ctype = u(i:i)
                read(u(1:i-1), '(I10)', iostat=ios) num
                if (ios /= 0) then
                    call set_error('Invalid unpacked numbered comet designation')
                    packed = ''
                    return
                end if
                if (num < 1 .or. num > 9999) then
                    call set_error('Comet number out of range (1-9999)')
                    packed = ''
                    return
                end if
                write(packed, '(I4.4,A1)') num, ctype
                return
            end if
        end do

        call set_error('Invalid unpacked numbered comet designation')
        packed = ''
    end function pack_comet_numbered

    !> Unpack full comet designation
    function unpack_comet_full(packed) result(unpacked)
        character(len=*), intent(in) :: packed
        character(len=30) :: unpacked
        character(len=20) :: p
        character(len=1) :: ctype
        character(len=8) :: prov_part
        integer :: plen

        call clear_error()
        p = trim_str(packed)
        plen = len_trim(p)

        if (plen == 8) then
            ctype = p(1:1)
            prov_part = p(2:8)

            if (index(COMET_TYPES, ctype) == 0) then
                call set_error('Invalid comet type')
                unpacked = ''
                return
            end if

            ! Check if asteroid-style (ends with uppercase)
            if (is_upper(prov_part(7:7))) then
                unpacked = ctype // '/' // trim(unpack_provisional(prov_part))
            else
                unpacked = ctype // '/' // trim(unpack_comet_provisional(prov_part(1:7)))
            end if
            return
        end if

        ! 9-char format: comet type + 8-char provisional with 2-letter fragment
        if (plen == 9) then
            ctype = p(1:1)
            prov_part = p(2:9)

            if (index(COMET_TYPES, ctype) == 0) then
                call set_error('Invalid comet type')
                unpacked = ''
                return
            end if

            ! 9-char always uses comet-style provisional with 2-letter fragment
            unpacked = ctype // '/' // trim(unpack_comet_provisional(prov_part))
            return
        end if

        call set_error('Invalid packed full comet designation length')
        unpacked = ''
    end function unpack_comet_full

    !> Pack full comet designation
    function pack_comet_full(unpacked) result(packed)
        character(len=*), intent(in) :: unpacked
        character(len=12) :: packed
        call clear_error()
        packed = ''
        call set_error('pack_comet_full not fully implemented')
    end function pack_comet_full

    !> Unpack satellite designation
    function unpack_satellite(packed) result(unpacked)
        character(len=*), intent(in) :: packed
        character(len=20) :: unpacked
        character(len=10) :: p
        character(len=1) :: century, planet
        character(len=2) :: year_str, num_encoded
        integer :: century_val, num

        call clear_error()
        p = trim_str(packed)

        if (len_trim(p) /= 8 .or. p(1:1) /= 'S') then
            call set_error('Invalid packed satellite designation')
            unpacked = ''
            return
        end if

        century = p(2:2)
        year_str = p(3:4)
        planet = p(5:5)
        num_encoded = p(6:7)

        if (index(SATELLITE_PLANETS, planet) == 0) then
            call set_error('Invalid planet code')
            unpacked = ''
            return
        end if

        century_val = get_century_code(century)
        if (MPCDesignationError) then
            unpacked = ''
            return
        end if

        num = decode_cycle_count(num_encoded)
        if (MPCDesignationError) then
            unpacked = ''
            return
        end if

        write(unpacked, '(A2,I2,A2,A1,A1,A1,I0)') 'S/', century_val, year_str, ' ', planet, ' ', num
    end function unpack_satellite

    !> Pack satellite designation
    function pack_satellite(unpacked) result(packed)
        character(len=*), intent(in) :: unpacked
        character(len=8) :: packed
        call clear_error()
        packed = ''
        call set_error('pack_satellite not fully implemented')
    end function pack_satellite

    !> Unpack ancient comet provisional
    function unpack_ancient_comet_provisional(packed) result(unpacked)
        character(len=*), intent(in) :: packed
        character(len=30) :: unpacked
        character(len=10) :: p
        character(len=1) :: ctype, half_month, fragment, prefix
        integer :: year, order_num

        call clear_error()
        p = trim_str(packed)

        if (len_trim(p) /= 8) then
            call set_error('Invalid ancient comet designation length')
            unpacked = ''
            return
        end if

        ctype = p(1:1)
        if (index(COMET_TYPES, ctype) == 0) then
            call set_error('Invalid comet type')
            unpacked = ''
            return
        end if

        ! Check for BCE prefix
        prefix = p(2:2)
        if (prefix == '/' .or. prefix == '.' .or. prefix == '-') then
            ! BCE year: decode complement encoding
            read(p(3:4), '(I2)') order_num  ! Temporarily use order_num for year code
            year = 99 - order_num
            if (prefix == '.') then
                year = year + 100
            else if (prefix == '-') then
                year = year + 200
            end if
            year = -year

            half_month = p(5:5)
            order_num = decode_cycle_count(p(6:7))
            if (MPCDesignationError) then
                unpacked = ''
                return
            end if
            fragment = p(8:8)

            if (fragment == '0') then
                write(unpacked, '(A1,A1,I0,A1,A1,I0)') ctype, '/', year, ' ', half_month, order_num
            else
                write(unpacked, '(A1,A1,I0,A1,A1,I0,A1,A1)') ctype, '/', year, ' ', half_month, order_num, '-', &
                    to_upper(fragment)
            end if
            return
        else
            ! Ancient year (< 1000)
            read(p(2:4), '(I3)') year
            half_month = p(5:5)
            order_num = decode_cycle_count(p(6:7))
            if (MPCDesignationError) then
                unpacked = ''
                return
            end if
            fragment = p(8:8)

            if (fragment == '0') then
                write(unpacked, '(A1,A1,I0,A1,A1,I0)') ctype, '/', year, ' ', half_month, order_num
            else
                write(unpacked, '(A1,A1,I0,A1,A1,I0,A1,A1)') ctype, '/', year, ' ', half_month, order_num, '-', &
                    to_upper(fragment)
            end if
        end if
    end function unpack_ancient_comet_provisional

    !> Detect format of a designation
    function detect_format(designation) result(info)
        character(len=*), intent(in) :: designation
        type(format_info) :: info
        character(len=80) :: des
        integer :: dlen
        character(len=1) :: first
        logical :: valid

        call clear_error()
        des = trim_str(designation)
        dlen = len_trim(des)
        info%format = ''
        info%dtype = ''
        info%subtype = ''

        if (dlen == 0) then
            call set_error('Empty designation')
            return
        end if

        ! Validate input for whitespace issues and invalid characters
        call validate_input(trim(designation), valid)
        if (.not. valid) return

        first = des(1:1)

        ! Check packed formats

        ! Packed permanent (5 chars)
        if (dlen == 5) then
            if (first == '~') then
                info%format = 'packed'
                info%dtype = 'permanent'
                info%subtype = 'permanent numbered (tilde/base-62, >= 620000)'
                return
            else if (is_all_digits(des(1:5))) then
                info%format = 'packed'
                info%dtype = 'permanent'
                info%subtype = 'permanent numbered (5-digit, < 100000)'
                return
            else if ((is_upper(first) .or. is_lower(first)) .and. is_all_digits(des(2:5))) then
                info%format = 'packed'
                info%dtype = 'permanent'
                info%subtype = 'permanent numbered (letter-prefix)'
                return
            else if (is_all_digits(des(1:4)) .and. (des(5:5) == 'P' .or. des(5:5) == 'D')) then
                info%format = 'packed'
                info%dtype = 'comet_numbered'
                info%subtype = 'comet numbered'
                return
            end if
        end if

        ! Packed provisional (7 chars)
        if (dlen == 7) then
            if (first == '_') then
                info%format = 'packed'
                info%dtype = 'provisional_extended'
                info%subtype = 'provisional (extended format, cycle >=620)'
                return
            else if (index(CENTURY_LETTERS, first) > 0 .and. is_upper(des(7:7)) .and. index(des(1:dlen), ' ') == 0) then
                info%format = 'packed'
                info%dtype = 'provisional'
                info%subtype = 'provisional'
                return
            else if (des(1:3) == 'PLS' .or. des(1:3) == 'T1S' .or. des(1:3) == 'T2S' .or. des(1:3) == 'T3S') then
                info%format = 'packed'
                info%dtype = 'survey'
                info%subtype = 'survey'
                return
            else if (index('IJKL', first) > 0 .and. (is_digit(des(7:7)) .or. is_lower(des(7:7)))) then
                info%format = 'packed'
                info%dtype = 'comet_provisional'
                info%subtype = 'comet provisional'
                return
            end if
        end if

        ! Packed satellite (8 chars starting with S, no spaces)
        if (dlen == 8 .and. first == 'S' .and. index(des(1:dlen), ' ') == 0) then
            info%format = 'packed'
            info%dtype = 'satellite'
            info%subtype = 'natural satellite'
            return
        end if

        ! Packed comet full (8 chars with comet type prefix, no spaces)
        if (dlen == 8 .and. index(COMET_TYPES, first) > 0 .and. index(des(1:dlen), ' ') == 0) then
            ! Check if it's asteroid-style or comet-style provisional
            if (is_upper(des(8:8))) then
                info%format = 'packed'
                info%dtype = 'comet_full'
                info%subtype = 'comet with provisional designation (8-char)'
                return
            else if (is_digit(des(2:2))) then
                info%format = 'packed'
                info%dtype = 'comet_ancient'
                info%subtype = 'comet with ancient provisional'
                return
            else if (des(2:2) == '/' .or. des(2:2) == '.' .or. des(2:2) == '-') then
                info%format = 'packed'
                info%dtype = 'comet_bce'
                info%subtype = 'comet with BCE year'
                return
            else
                info%format = 'packed'
                info%dtype = 'comet_full'
                info%subtype = 'comet with provisional designation (8-char)'
                return
            end if
        end if

        ! Packed comet with 2-letter fragment (9 chars)
        if (dlen == 9 .and. index(COMET_TYPES, first) > 0 .and. index(des(1:dlen), ' ') == 0) then
            ! Check for format: comet type + century letter + 2-digit year + halfmonth + 2-char order + 2-char fragment
            if (is_upper(des(2:2)) .and. is_lower(des(8:8)) .and. is_lower(des(9:9))) then
                info%format = 'packed'
                info%dtype = 'comet_full'
                info%subtype = 'comet with provisional designation (9-char, fragment)'
                return
            end if
        end if

        ! --- UNPACKED FORMATS ---

        ! Unpacked permanent (all digits)
        if (is_all_digits(des(1:dlen))) then
            info%format = 'unpacked'
            info%dtype = 'permanent'
            info%subtype = 'permanent numbered'
            return
        end if

        ! Unpacked satellite: "S/YYYY P N"
        if (dlen >= 10 .and. des(1:2) == 'S/') then
            info%format = 'unpacked'
            info%dtype = 'satellite'
            info%subtype = 'natural satellite'
            return
        end if

        ! Unpacked numbered comet: "NP" or "ND"
        if (dlen >= 2 .and. dlen <= 5) then
            if (is_all_digits(des(1:dlen-1)) .and. (des(dlen:dlen) == 'P' .or. des(dlen:dlen) == 'D')) then
                info%format = 'unpacked'
                info%dtype = 'comet_numbered'
                info%subtype = 'comet numbered'
                return
            end if
        end if

        ! Unpacked comet with prefix: "C/YYYY ..." or ancient "C/YYY ..."
        if (dlen >= 7 .and. index(COMET_TYPES, first) > 0 .and. des(2:2) == '/') then
            info%format = 'unpacked'
            info%dtype = 'comet_full'
            info%subtype = 'comet provisional'
            return
        end if

        ! Old-style provisional: "AYYY HH" or "BYYY HH" (A=1800s, B=1900s)
        if (dlen >= 7 .and. (first == 'A' .or. first == 'B') .and. &
            is_all_digits(des(2:4)) .and. des(5:5) == ' ' .and. is_upper(des(6:6))) then
            info%format = 'unpacked'
            info%dtype = 'provisional'
            info%subtype = 'old-style provisional'
            return
        end if

        ! Unpacked provisional: "YYYY LL" or "YYYY LLN"
        if (dlen >= 7 .and. is_all_digits(des(1:4)) .and. des(5:5) == ' ' .and. is_upper(des(6:6))) then
            info%format = 'unpacked'
            info%dtype = 'provisional'
            info%subtype = 'provisional'
            return
        end if

        ! Unpacked survey: "NNNN P-L" or "NNNN T-N"
        if (dlen >= 6 .and. index(des, ' P-L') > 0) then
            info%format = 'unpacked'
            info%dtype = 'survey'
            info%subtype = 'survey (Palomar-Leiden)'
            return
        end if
        if (dlen >= 6 .and. index(des, ' T-') > 0) then
            info%format = 'unpacked'
            info%dtype = 'survey'
            info%subtype = 'survey (Trojan)'
            return
        end if

        call set_error('Unable to detect designation format: ' // trim(designation))
    end function detect_format

    !> Convert designation between formats
    function convert(designation) result(res)
        character(len=*), intent(in) :: designation
        type(conversion_result) :: res
        type(format_info) :: info
        character(len=80) :: output
        integer :: num, ios

        call clear_error()
        res%input = trim(designation)
        res%output = ''

        info = detect_format(designation)
        if (MPCDesignationError) then
            res%info = info
            return
        end if

        res%info = info

        if (info%format == 'packed') then
            select case (trim(info%dtype))
                case ('permanent')
                    num = unpack_permanent(designation)
                    if (.not. MPCDesignationError) write(res%output, '(I0)') num
                case ('provisional', 'survey')
                    res%output = trim(unpack_provisional(designation))
                case ('provisional_extended')
                    res%output = trim(unpack_extended_provisional(designation))
                case ('comet_numbered')
                    res%output = trim(unpack_comet_numbered(designation))
                case ('comet_provisional')
                    res%output = trim(unpack_comet_provisional(designation))
                case ('comet_full')
                    res%output = trim(unpack_comet_full(designation))
                case ('comet_ancient', 'comet_bce')
                    res%output = trim(unpack_ancient_comet_provisional(designation))
                case ('satellite')
                    res%output = trim(unpack_satellite(designation))
                case default
                    call set_error('Unknown type: ' // trim(info%dtype))
            end select
        else
            select case (trim(info%dtype))
                case ('permanent')
                    read(designation, '(I20)', iostat=ios) num
                    if (ios /= 0) then
                        call set_error('Invalid asteroid number')
                        return
                    end if
                    res%output = pack_permanent(num)
                case ('provisional', 'survey')
                    res%output = pack_provisional_impl(designation)
                case ('comet_numbered')
                    res%output = pack_comet_numbered(designation)
                case ('comet_full')
                    res%output = pack_comet_full_impl(designation)
                case ('satellite')
                    res%output = pack_satellite_impl(designation)
                case default
                    call set_error('Unknown type: ' // trim(info%dtype))
            end select
        end if
    end function convert

    !> Simple conversion - just return output string
    function convert_simple(designation) result(output)
        character(len=*), intent(in) :: designation
        character(len=80) :: output
        type(conversion_result) :: res

        res = convert(designation)
        output = res%output
    end function convert_simple

    !> Ensure designation is in packed format
    function pack_designation(designation) result(packed)
        character(len=*), intent(in) :: designation
        character(len=80) :: packed
        type(format_info) :: info

        call clear_error()
        info = detect_format(designation)

        if (MPCDesignationError) then
            packed = ''
            return
        end if

        if (info%format == 'packed') then
            packed = trim_str(designation)
        else
            packed = convert_simple(designation)
        end if
    end function pack_designation

    !> Ensure designation is in unpacked format
    function unpack_designation(designation) result(unpacked)
        character(len=*), intent(in) :: designation
        character(len=80) :: unpacked
        type(format_info) :: info

        call clear_error()
        info = detect_format(designation)

        if (MPCDesignationError) then
            unpacked = ''
            return
        end if

        if (info%format == 'unpacked') then
            unpacked = trim_str(designation)
        else
            unpacked = convert_simple(designation)
        end if
    end function unpack_designation

    !> Check if designation is valid
    logical function is_valid_designation(designation)
        character(len=*), intent(in) :: designation
        type(format_info) :: info

        call clear_error()
        info = detect_format(designation)
        is_valid_designation = .not. MPCDesignationError
        call clear_error()  ! Clear any error set during detection
    end function is_valid_designation

    !> Implementation of pack_provisional
    function pack_provisional_impl(unpacked) result(packed)
        character(len=*), intent(in) :: unpacked
        character(len=7) :: packed
        character(len=40) :: u
        integer :: i, year, century, order_num, ios, survey_num
        character(len=1) :: century_code, half_month, second_letter
        character(len=2) :: year_short, order_encoded

        call clear_error()
        u = trim_str(unpacked)

        ! Check for survey designations
        i = index(u, ' P-L')
        if (i > 0) then
            read(u(1:i-1), '(I10)', iostat=ios) survey_num
            if (ios == 0 .and. survey_num >= 1) then
                write(packed, '(A3,I4.4)') 'PLS', survey_num
                return
            end if
        end if

        i = index(u, ' T-1')
        if (i > 0) then
            read(u(1:i-1), '(I10)', iostat=ios) survey_num
            if (ios == 0 .and. survey_num >= 1) then
                write(packed, '(A3,I4.4)') 'T1S', survey_num
                return
            end if
        end if

        i = index(u, ' T-2')
        if (i > 0) then
            read(u(1:i-1), '(I10)', iostat=ios) survey_num
            if (ios == 0 .and. survey_num >= 1) then
                write(packed, '(A3,I4.4)') 'T2S', survey_num
                return
            end if
        end if

        i = index(u, ' T-3')
        if (i > 0) then
            read(u(1:i-1), '(I10)', iostat=ios) survey_num
            if (ios == 0 .and. survey_num >= 1) then
                write(packed, '(A3,I4.4)') 'T3S', survey_num
                return
            end if
        end if

        ! Check for old-style designations: AYYY HM or BYYY HM (A/B prefix, 3-digit year)
        if ((u(1:1) == 'A' .or. u(1:1) == 'B') .and. is_all_digits(u(2:4)) .and. u(5:5) == ' ') then
            read(u(2:4), '(I3)', iostat=ios) year
            if (ios == 0) then
                ! Determine century from first digit of YYY
                if (u(2:2) == '8') then
                    century = 18  ! 1800s
                else if (u(2:2) == '9') then
                    century = 19  ! 1900s
                else
                    call set_error('Invalid old-style year')
                    packed = ''
                    return
                end if
                year = century * 100 + mod(year, 100)
                century_code = get_century_letter(century)
                if (MPCDesignationError) then
                    packed = ''
                    return
                end if
                year_short = u(3:4)
                half_month = u(6:6)
                second_letter = u(7:7)

                ! Validate
                if (.not. is_valid_half_month(half_month)) then
                    call set_error('Invalid half-month letter')
                    packed = ''
                    return
                end if

                ! Get order number if present
                if (len_trim(u) > 7) then
                    read(u(8:), '(I10)', iostat=ios) order_num
                    if (ios /= 0) order_num = 0
                else
                    order_num = 0
                end if

                order_encoded = encode_cycle_count(order_num)
                if (MPCDesignationError) then
                    packed = ''
                    return
                end if

                packed = century_code // year_short // half_month // order_encoded // second_letter
                return
            end if
        end if

        ! Check for invalid survey-like patterns (X-L, T-4, etc.)
        ! Valid patterns: P-L, T-1, T-2, T-3
        i = index(u, '-')
        if (i > 0 .and. i > 4) then
            ! Has a dash after position 4, could be survey-like
            if (index(u, ' P-L') == 0 .and. index(u, ' T-1') == 0 .and. &
                index(u, ' T-2') == 0 .and. index(u, ' T-3') == 0) then
                ! Not a valid survey pattern - check if it looks like one
                if (u(i-1:i-1) >= 'A' .and. u(i-1:i-1) <= 'Z' .and. &
                    u(i+1:i+1) >= 'A' .and. u(i+1:i+1) <= 'Z') then
                    call set_error('Invalid survey designation code')
                    packed = ''
                    return
                else if (u(i-1:i-1) == 'T' .and. is_digit(u(i+1:i+1))) then
                    call set_error('Invalid survey designation number')
                    packed = ''
                    return
                end if
            end if
        end if

        ! Standard provisional: "YYYY HM" or "YYYY HMN"
        read(u(1:4), '(I4)', iostat=ios) year
        if (ios /= 0) then
            call set_error('Invalid unpacked provisional designation')
            packed = ''
            return
        end if

        century = year / 100
        year_short = u(3:4)
        half_month = u(6:6)
        second_letter = u(7:7)

        if (.not. is_valid_half_month(half_month)) then
            call set_error('Invalid half-month letter')
            packed = ''
            return
        end if

        ! Second letter must be a letter (not a digit)
        if (.not. (is_upper(second_letter) .or. is_lower(second_letter))) then
            call set_error('Second letter must be alphabetic')
            packed = ''
            return
        end if

        ! Case consistency: both letters should be same case
        if ((is_upper(half_month) .and. is_lower(second_letter)) .or. &
            (is_lower(half_month) .and. is_upper(second_letter))) then
            call set_error('Mixed case letters in designation')
            packed = ''
            return
        end if

        ! Check for three consecutive letters (like XAB instead of XA or XA1)
        if (len_trim(u) >= 8) then
            if (is_upper(u(8:8)) .or. is_lower(u(8:8))) then
                call set_error('Invalid designation format: three consecutive letters')
                packed = ''
                return
            end if
        end if

        century_code = get_century_letter(century)
        if (MPCDesignationError) then
            packed = ''
            return
        end if

        ! Parse order number
        if (len_trim(u) > 7) then
            read(u(8:), '(I10)', iostat=ios) order_num
            if (ios /= 0) order_num = 0
        else
            order_num = 0
        end if

        ! Check if extended format needed
        if (order_num >= 620) then
            packed = pack_extended_provisional_impl(year, half_month, second_letter, order_num)
            return
        end if

        order_encoded = encode_cycle_count(order_num)
        if (MPCDesignationError) then
            packed = ''
            return
        end if

        packed = century_code // year_short // half_month // order_encoded // second_letter
    end function pack_provisional_impl

    !> Pack extended provisional
    function pack_extended_provisional_impl(year, half_month, second_letter, cycle) result(packed)
        integer, intent(in) :: year, cycle
        character(len=1), intent(in) :: half_month, second_letter
        character(len=7) :: packed
        integer :: year_short, letter_pos, base_seq

        year_short = mod(year, 100)
        letter_pos = letter_to_position(second_letter)
        base_seq = (cycle - 620) * 25 + letter_pos - 1

        packed = '_' // num_to_base62(year_short) // half_month // num_to_base62_string(base_seq, 4)
    end function pack_extended_provisional_impl

    !> Implementation of pack_comet_full
    function pack_comet_full_impl(unpacked) result(packed)
        character(len=*), intent(in) :: unpacked
        character(len=15) :: packed  ! Longer for 2-letter fragments
        character(len=40) :: u
        integer :: i, year, order_num, ios, space_pos, abs_year, bce_code, dash_pos, frag_len
        character(len=1) :: ctype, half_month, bce_prefix, fragment, fragment2
        character(len=20) :: prov_part, year_str
        character(len=10) :: prov_packed  ! 10 chars for type + 2-letter fragments

        call clear_error()
        u = trim_str(unpacked)

        ! Find the type character before /
        i = index(u, '/')
        if (i == 0 .or. i < 2) then
            call set_error('Invalid unpacked comet designation')
            packed = ''
            return
        end if

        ctype = u(i-1:i-1)
        if (index(COMET_TYPES, ctype) == 0) then
            call set_error('Invalid comet type')
            packed = ''
            return
        end if

        ! Find the space after the year
        space_pos = index(u(i+1:), ' ')
        if (space_pos == 0) then
            call set_error('Invalid comet designation format')
            packed = ''
            return
        end if
        space_pos = space_pos + i  ! Adjust for offset

        ! Get year string (may be negative for BCE)
        year_str = u(i+1:space_pos-1)
        read(year_str, '(I10)', iostat=ios) year
        if (ios /= 0) then
            call set_error('Invalid comet year')
            packed = ''
            return
        end if

        ! Get provisional part (after space)
        prov_part = trim(u(space_pos+1:))

        ! Check for fragment (indicated by hyphen)
        dash_pos = index(prov_part, '-')
        fragment = '0'  ! Default: no fragment
        fragment2 = ' '  ! Default: no second fragment char
        frag_len = 0
        if (dash_pos > 0) then
            ! Fragment must be 1-2 uppercase letters
            frag_len = len_trim(prov_part) - dash_pos
            if (frag_len < 1 .or. frag_len > 2) then
                call set_error('Fragment must be 1-2 letters')
                packed = ''
                return
            end if
            if (.not. is_upper(prov_part(dash_pos+1:dash_pos+1))) then
                call set_error('Fragment must be a letter')
                packed = ''
                return
            end if
            if (frag_len == 2 .and. .not. is_upper(prov_part(dash_pos+2:dash_pos+2))) then
                call set_error('Fragment must be all letters')
                packed = ''
                return
            end if
            ! Extract fragment (convert to lowercase for packed format)
            fragment = char(ichar(prov_part(dash_pos+1:dash_pos+1)) + 32)
            if (frag_len == 2) then
                fragment2 = char(ichar(prov_part(dash_pos+2:dash_pos+2)) + 32)
            end if
            ! Truncate prov_part to exclude fragment for further parsing
            prov_part = prov_part(1:dash_pos-1)
        end if

        ! Check for BCE or ancient year
        if (year < 0) then
            ! BCE year: use complement encoding
            abs_year = abs(year)
            bce_code = 99 - mod(abs_year, 100)
            if (abs_year < 100) then
                bce_prefix = '/'
            else if (abs_year < 200) then
                bce_prefix = '.'
            else if (abs_year < 300) then
                bce_prefix = '-'
            else
                call set_error('BCE year out of supported range')
                packed = ''
                return
            end if

            half_month = prov_part(1:1)
            read(prov_part(2:), '(I10)', iostat=ios) order_num
            if (ios /= 0) order_num = 1

            write(packed, '(A1,A1,I2.2,A1,A2,A1)') ctype, bce_prefix, bce_code, &
                  half_month, encode_cycle_count(order_num), fragment
            return
        else if (year < 1000) then
            ! Ancient year (1-999): use 3-digit year
            half_month = prov_part(1:1)
            read(prov_part(2:), '(I10)', iostat=ios) order_num
            if (ios /= 0) order_num = 1

            write(packed, '(A1,I3.3,A1,A2,A1)') ctype, year, half_month, &
                  encode_cycle_count(order_num), fragment
            return
        end if

        ! Standard year (>= 1000)
        ! Check for asteroid-style vs comet-style
        if (len_trim(prov_part) >= 2 .and. is_upper(prov_part(2:2))) then
            ! Asteroid-style: pack as provisional
            prov_packed = pack_provisional_impl(u(i+1:))
        else
            ! Comet-style: read half-month and order
            half_month = prov_part(1:1)
            read(prov_part(2:), '(I10)', iostat=ios) order_num
            if (ios /= 0) order_num = 1

            if (order_num < 1) then
                call set_error('Comet order number must be positive')
                packed = ''
                return
            end if

            ! Simple packing for comet provisional
            prov_packed = get_century_letter(year/100) // year_str(3:4) // half_month // &
                          encode_cycle_count(order_num) // fragment
            if (frag_len == 2) then
                prov_packed = trim(prov_packed) // fragment2
            end if
        end if

        if (MPCDesignationError) then
            packed = ''
            return
        end if

        packed = ctype // trim(prov_packed)
    end function pack_comet_full_impl

    !> Implementation of pack_satellite
    function pack_satellite_impl(unpacked) result(packed)
        character(len=*), intent(in) :: unpacked
        character(len=8) :: packed
        character(len=30) :: u
        integer :: year, num, century, ios
        character(len=1) :: planet

        call clear_error()
        u = trim_str(unpacked)

        ! Parse "S/YYYY P N"
        if (u(1:2) /= 'S/') then
            call set_error('Invalid unpacked satellite designation')
            packed = ''
            return
        end if

        read(u(3:6), '(I4)', iostat=ios) year
        if (ios /= 0) then
            call set_error('Invalid satellite year')
            packed = ''
            return
        end if

        planet = u(8:8)
        if (index(SATELLITE_PLANETS, planet) == 0) then
            call set_error('Invalid planet code')
            packed = ''
            return
        end if

        read(u(10:), '(I10)', iostat=ios) num
        if (ios /= 0) then
            call set_error('Invalid satellite number')
            packed = ''
            return
        end if

        if (num < 1) then
            call set_error('Satellite number must be positive')
            packed = ''
            return
        end if

        century = year / 100
        packed = 'S' // get_century_letter(century) // u(5:6) // planet // encode_cycle_count(num) // '0'
    end function pack_satellite_impl

end module mpc_designation
