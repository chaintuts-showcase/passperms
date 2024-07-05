! This module provides core PassPerm calculation functionality
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

	CONTAINS

	! This subroutine calculates the results array for the given complexity (character space)
	! It will calculate an entry for each complexity space to the sample-length power
	SUBROUTINE calc_complexities(results)

		! Declare function params and returns, and data
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

END MODULE