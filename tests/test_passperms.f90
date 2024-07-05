! This file contains simple unit tests for PassPerms functionality
!
! Author: Josh McIntyre
!

PROGRAM test_passperms

	USE pass_data
	
	IMPLICIT NONE
	REAL(8), DIMENSION(ASIZE)::results
	INTEGER::res
	INTEGER::i

	CALL calc_lengths(results)

	res = 0

	! Loop on each element and exponentiate
	DO i = 1, ASIZE, 1
		IF (results(i) /= 62 ** LENGTHS(i)) THEN
			res = 1
		ENDIF
	END DO
	
	IF (res == 0) THEN
		WRITE(*, *) "Tests passed"
	ELSE
		WRITE(*, *) "Tests failed"
	ENDIF

END PROGRAM