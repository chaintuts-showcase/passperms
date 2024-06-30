! This program calculates permutations for passwords with varying lengths & complexities
! This is designed to show the security of length over complexity
!
! Author: Josh McIntyre
!

! Define data parameters in a module
MODULE pass_data

    ! Constant declarations for character spaces and lengths
    INTEGER, PARAMETER::ASIZE = 7

    ! numerical (10), lowercase, upper lower, upper lower numerical (62), various amounts of symbols
    REAL, DIMENSION(ASIZE), PARAMETER::COMPLEXITIES = (/ 10, 26, 52, 62, 72, 82, 92 /)
    REAL, PARAMETER::LENGTH_SAMPLE = 8
    
    ! Lengths of the password in characters
    REAL, DIMENSION(ASIZE), PARAMETER::LENGTHS = (/ 8, 9, 10, 12, 14, 16, 20 /)
    REAL, PARAMETER::COMPLEXITY_SAMPLE = 62

    ! Password cracking constants
    ! 180 billion (giga) hashes per second - professional 25 GPU cluster, MD5 algorithm
    ! 7.652 billion (giga) hashes per second - consumer laptop with Nvidia GeForce 1050 Ti GPU
    REAL, PARAMETER::HASHPOWER_PRO = 180000000000.0
    REAL, PARAMETER::HASHPOWER_LAPTOP = 7652000000.0
    REAL, PARAMETER::SECONDS_PER_MINUTE = 60.0
    REAL, PARAMETER::SECONDS_PER_HOUR = 3600.0
    REAL, PARAMETER::SECONDS_PER_DAY = 86400.0
    REAL, PARAMETER::SECONDS_PER_YEAR = 31557600.0
    
    ! Output constants
    INTEGER, PARAMETER::HSIZE = 100

    CHARACTER(HSIZE), PARAMETER::COMPLEXITIES_LABEL = "Combinations for constant length 8, variable complexity"
    CHARACTER(HSIZE), PARAMETER::COMPLEXITIES_HEADER = "Complexity           Combinations            Crack Time"

    CHARACTER(HSIZE), PARAMETER::CRACKTIME_LABEL_PRO = "Cracking w/ 180 GH/s, MD5 - 25 GPU professional cluster"
    CHARACTER(HSIZE), PARAMETER::CRACKTIME_LABEL_LAPTOP = "Cracking w/ 7.652 GH/s, MD5 - NVidia GeForce 1050 Ti consumer laptop"

    CHARACTER(HSIZE), PARAMETER::LENGTHS_LABEL = "Combinations for constant complexity 62, variable length"
    CHARACTER(HSIZE), PARAMETER::LENGTHS_HEADER = "Length           Combinations            Crack Time"

END MODULE

! This is the main entry point for the program
PROGRAM passperms

    ! Data declarations for command line args
    USE pass_data

    IMPLICIT NONE
    CHARACTER(15)::flag, flag2
    
    ! Data declarations for calculations and results
    REAL(8), DIMENSION(ASIZE)::results, times
    CHARACTER(HSIZE)::cracktime_label

    ! Calculate and output just the permutations with no guess time specified
    ! Otherwise, calculate and output the crack time matrix
    IF (IARGC() == 1 .OR. IARGC() == 2) THEN

        ! Fetch arguments
        CALL getarg(1, flag)
        CALL getarg(2, flag2)
        IF (flag == "-h" .OR. flag == "--help") THEN
            CALL output_help()
        ELSE IF (flag == "-c" .OR. flag == "--complexities") THEN
            CALL calc_complexities(results)
            IF (flag2 == "-f" .OR. flag2 == "--fast") THEN
                CALL calc_times(results, HASHPOWER_PRO, times)
                cracktime_label = CRACKTIME_LABEL_PRO
            ELSE
                ! Default to consumer laptop cracking time
                CALL calc_times(results, HASHPOWER_LAPTOP, times)
                cracktime_label = CRACKTIME_LABEL_LAPTOP
            END IF
            CALL output_results(COMPLEXITIES_LABEL, cracktime_label, COMPLEXITIES_HEADER, COMPLEXITIES, results, times)
        ELSE IF (flag == "-l" .OR. flag == "--lengths") THEN
            CALL calc_lengths(results)
            IF (flag2 == "-f" .OR. flag2 == "--fast") THEN
                CALL calc_times(results, HASHPOWER_PRO, times)
                cracktime_label = CRACKTIME_LABEL_PRO
            ELSE
                ! Default to consumer laptop cracking time
                CALL calc_times(results, HASHPOWER_LAPTOP, times)
                cracktime_label = CRACKTIME_LABEL_LAPTOP
            END IF
            CALL output_results(LENGTHS_LABEL, cracktime_label, LENGTHS_HEADER, LENGTHS, results, times)
        END IF
    ELSE
        CALL output_help()
    END IF

END PROGRAM

! This subroutine calculates the results array for the given complexity (character space)
! It will calculate an entry for each complexity space to the sample-length power
SUBROUTINE calc_complexities(results)

    ! Declare function params and returns, and data
    USE pass_data

    IMPLICIT none
    REAL(8), DIMENSION(ASIZE), INTENT(OUT)::results

    ! Can just use array per-element shorthand here
    results = COMPLEXITIES ** LENGTH_SAMPLE

    RETURN
END SUBROUTINE calc_complexities

! This subroutine calculates the results array for the given lengths
! It will calculate an entry for one sample-complexity to the length power
SUBROUTINE calc_lengths(results)

    ! Declare function params and returns, and data
    USE pass_data

    IMPLICIT none
    REAL(8), DIMENSION(ASIZE), INTENT(OUT)::results

    INTEGER::i

    ! Loop on each element and exponentiate; array shorthand does not work in reverse
    DO i = 1, ASIZE, 1
        results(i) = COMPLEXITY_SAMPLE ** LENGTHS(i)
    END DO

    RETURN
END SUBROUTINE calc_lengths

! This subroutine will calculate cracking times for a given result set and hashing power
SUBROUTINE calc_times(results, hash, times)

    ! Declare function params and returns, and data
    USE pass_data

    IMPLICIT none
    REAL(8), DIMENSION(ASIZE), INTENT(IN)::results
    REAL, INTENT(IN)::hash
    REAL(8), DIMENSION(ASIZE), INTENT(OUT)::times

    REAL::optime

    ! Calculate the time per one operation, based on Gigahashes per second
    optime = 1.0 / hash

    ! Calculate times per element
    times = results * optime

    RETURN
END SUBROUTINE calc_times

! This subroutine outputs the results of the calculations
SUBROUTINE output_results(label, cracktime_label, header, bases, results, times)

    ! Declare function params and returns, and data
    USE pass_data

    IMPLICIT none
    CHARACTER(HSIZE), INTENT(IN)::label, cracktime_label, header
    REAL, DIMENSION(ASIZE), INTENT(IN)::bases
    REAL(8), DIMENSION(ASIZE), INTENT(IN)::results, times
    
    INTEGER::i

    WRITE(*, *) label
    WRITE(*, *) cracktime_label
    WRITE(*, *) header

    DO i = 1, ASIZE, 1
    
        IF (times(i) .LE. SECONDS_PER_MINUTE) THEN
            WRITE(*, 100) bases(i), results(i), times(i)
            100 FORMAT(3x, F3.0, 5x, ES20.0, F20.5, " seconds")
        ELSE IF (times(i) .LE. SECONDS_PER_HOUR) THEN
            WRITE(*, 200) bases(i), results(i), times(i) / SECONDS_PER_MINUTE
            200 FORMAT(3x, F3.0, 5x, ES20.0, F20.5, " minutes")
        ELSE IF (times(i) .LE. SECONDS_PER_DAY) THEN
            WRITE(*, 300) bases(i), results(i), times(i) / SECONDS_PER_HOUR
            300 FORMAT(3x, F3.0, 5x, ES20.0, F20.5, " hours")
        ELSE IF (times(i) .LE. SECONDS_PER_YEAR) THEN
            WRITE(*, 400) bases(i), results(i), times(i) / SECONDS_PER_DAY
            400 FORMAT(3x, F3.0, 5x, ES20.0, F20.5, " days")
        ELSE IF (times(i) .LE. SECONDS_PER_YEAR * 10000) THEN
            WRITE(*, 500) bases(i), results(i), times(i) / SECONDS_PER_YEAR
            500 FORMAT(3x, F3.0, 5x, ES20.0, F20.5, " years")
        ELSE
            WRITE(*, 600) bases(i), results(i), times(i) / SECONDS_PER_YEAR
            600 FORMAT(3x, F3.0, 5x, ES20.0, ES20.5, " years")
        END IF
        
    END DO

    RETURN
END SUBROUTINE output_results

! This subroutine prints a help message
SUBROUTINE output_help()
    WRITE(*, *) "Usage: ./passperms [-p/--perms] [-h/--help] "
    WRITE(*, *) "-c/--complexities - Show the number of possible permutations for a constant length and variable complexities"
    WRITE(*, *) "-l/--lengths - Show the number of possible permutations for a constant complexity and variable lengths"
    WRITE(*, *) "-f/--fast - Show professional grade crack times (180 GH/s, MD5) 25 GPU cluster"
    WRITE(*, *) "-r/--regular - Show consumer grade crack times (7.652 GH/s, MD5) Nvidia GeForce 1050 Ti"
    WRITE(*, *) "-h/--help - Show this help message"
    
    STOP
END SUBROUTINE output_help