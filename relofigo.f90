program RomanClock
    implicit none
    integer :: hours, minutes, seconds
    character(len=10) :: roman_hours, roman_minutes, roman_seconds
 
    do
        call get_system_time(hours, minutes, seconds)
        roman_hours = convert_to_roman(hours)
        roman_minutes = convert_to_roman(minutes)
        roman_seconds = convert_to_roman(seconds)
 
        write(*, '(A)', advance='no') char(13) // "Current Time: " // trim(roman_hours) // ":" // trim(roman_minutes) // ":" // trim(roman_seconds)
        call sleep(1)
    end do
contains
 
    subroutine get_system_time(h, m, s)
        implicit none
        integer, intent(out) :: h, m, s
        integer :: values(8)
        call date_and_time(values=values)
        h = values(5)  ! Hours
        m = values(6)  ! Minutes
        s = values(7)  ! Seconds
    end subroutine get_system_time
 
    function convert_to_roman(num) result(roman)
        implicit none
        integer, intent(in) :: num
        character(len=10) :: roman
 
        character(len=10), dimension(60) :: roman_numerals
        data roman_numerals / "nulla", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", &
            "X", "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", "XVIII", "XIX", "XX", "XXI", "XXII", "XXIII", &
            "XXIV", "XXV", "XXVI", "XXVII", "XXVIII", "XXIX", "XXX", "XXXI", "XXXII", "XXXIII", "XXXIV", &
            "XXXV", "XXXVI", "XXXVII", "XXXVIII", "XXXIX", "XL", "XLI", "XLII", "XLIII", "XLIV", "XLV", &
            "XLVI", "XLVII", "XLVIII", "XLIX", "L", "LI", "LII", "LIII", "LIV", "LV", "LVI", "LVII", "LVIII", "LIX" /
 
        if (num >= 0 .and. num < 60) then
            roman = roman_numerals(num + 1)  ! Adjust for 1-based index
        else
            roman = "ERR"  ! Error case
        end if
    end function convert_to_roman
 
end program RomanClock
