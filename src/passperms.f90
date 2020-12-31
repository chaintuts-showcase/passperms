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
    
    ! Output constants
    INTEGER, PARAMETER::HSIZE = 100

    CHARACTER(HSIZE), PARAMETER::COMPLEXITIES_LABEL = "Combinations for constant length 8, variable complexity"
    CHARACTER(HSIZE), PARAMETER::COMPLEXITIES_HEADER = "Complexity       Combinations"

    CHARACTER(HSIZE), PARAMETER::LENGTHS_LABEL = "Combinations for constant complexity 62, variable length"
    CHARACTER(HSIZE), PARAMETER::LENGTHS_HEADER = "Length       Combinations"

END MODULE

! This is the main entry point for the program
PROGRAM passperms

    ! Data declarations for command line args
    USE pass_data

    IMPLICIT NONE
    CHARACTER(15)::flag
    
    ! Data declarations for calculations and results
    REAL(8), DIMENSION(ASIZE)::results

    ! Calculate and output just the permutations with no guess time specified
    ! Otherwise, calculate and output the crack time matrix
    IF (IARGC() == 1) THEN

        ! Fetch arguments
        CALL getarg(1, flag)
        IF (flag == "-h" .OR. flag == "--help") THEN
            CALL output_help()
        ELSE IF (flag == "-c" .OR. flag == "--complexities") THEN
            CALL calc_complexities(results)
            CALL output_results(COMPLEXITIES_LABEL, COMPLEXITIES_HEADER, COMPLEXITIES, results)
        ELSE IF (flag == "-l" .OR. flag == "--lengths") THEN
            CALL calc_lengths(results)
            CALL output_results(LENGTHS_LABEL, LENGTHS_HEADER, LENGTHS, results)
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

! This subroutine will calculate cracking times for a given 

! This subroutine outputs the results of the calculations
SUBROUTINE output_results(label, header, bases, results)

    ! Declare function params and returns, and data
    USE pass_data

    IMPLICIT none
    CHARACTER(HSIZE), INTENT(IN)::label, header
    REAL, DIMENSION(ASIZE), INTENT(IN)::bases
    REAL(8), DIMENSION(ASIZE), INTENT(IN)::results
    
    INTEGER::i
    REAL::trillion = 1000000000000.

    WRITE(*, *) label
    WRITE(*, *) header

    DO i = 1, ASIZE, 1
    
        IF (results(i) .LE. trillion) THEN
            WRITE(*, 200) bases(i), results(i)
            200 FORMAT(3x, F3.0, 5x, F20.0)
        ELSE
            WRITE(*, 300) bases(i), results(i)
            300 FORMAT(3x, F3.0, 5x, ES20.5)    
        END IF
        
    END DO

    RETURN
END SUBROUTINE output_results

! This subroutine prints a help message
SUBROUTINE output_help()
    WRITE(*, *) "Usage: ./passperms [-p/--perms] [-h/--help] "
    WRITE(*, *) "-c/--complexities - Show the number of possible permutations for a constant length and variable complexities"
    WRITE(*, *) "-l/--lengths - Show the number of possible permutations for a constant complexity and variable lengths"
    WRITE(*, *) "-h/--help - Show this help message"
    
    STOP
END SUBROUTINE output_help