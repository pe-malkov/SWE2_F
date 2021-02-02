      DOUBLE PRECISION FUNCTION DSECND( )
!
!  -- LAPACK auxiliary routine (version 3.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     September 30, 1994
!
!  Purpose
!  =======
!
!  DSECND returns the user time for a process in seconds.
!  This version gets the time from the system function ETIME.
!
! =====================================================================
!
!     .. Local Scalars ..
      REAL               T1
!     ..
!     .. Local Arrays ..
#ifdef fortran77
      REAL               TARRAY( 2 )
#endif
!     ..
!     .. External Functions ..
      REAL               ETIME
      EXTERNAL           ETIME
!     ..
!     .. Executable Statements ..
!
#ifdef fortran77
      T1 = ETIME( TARRAY )
      DSECND = TARRAY( 1 )
#endif
#ifdef fortran90
      call cpu_time(T1)
      DSECND = T1
#endif
      RETURN
!
!     End of DSECND
!
      END
