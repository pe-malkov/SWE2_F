      SUBROUTINE PRINTR(N,COM1,CM,IR,IC,IRU,ICU,IRS,ICS,NROWP,ICLMAX)
C-----
C-----I----------------------------------------------------------------I
C-----
COMMENT PRINTOUT OF A REAL (IR X IC)-MATRIX
C-----
C-----I----------------------------------------------------------------I
C-----
      IMPLICIT NONE
C
C     ..Global variables..
C
      INTEGER   IC, ICLMAX, ICS, ICU, IR, IRS, IRU, N, NROWP
      CHARACTER(len=16) COM1
      REAL   CM(IR,IC)
C
C     ..Local variables..
C
      INTEGER   ICL, IPCNTL, IRL
      CHARACTER(len=16) COM2
      CHARACTER(len=23) COM3
c-----
c     .. end of declaration part ..
c-----
c-----I----------------------------------------------------------------I
c-----
c     .. begin of subroutine statements ..
c-----
      IPCNTL = 6
C
      COM2='      '
      COM3='   CONTROL PRINTOUT OF '
      IF(N.GT.16)N=16
      COM2(1:N) = COM1(1:N)
      WRITE(IPCNTL,*)
      WRITE(IPCNTL,*)COM3,COM2
      WRITE(IPCNTL,1000)IR,IC
      WRITE(IPCNTL,2000)IRU,ICU
      WRITE(IPCNTL,3000)IRS,ICS
C
      WRITE(IPCNTL,5000)(ICS+icl,ICL=0,ICLMAX-1)
COMMENT The print starts on IRS-row and ICS-columns. Prints out
C       NROWP-rows of 0,...,9 = 10 columns
C                                      \
      DO 300 IRL = IRS, IRS + NROWP - 1
C                                          \
c       WRITE(IPCNTL,4000)(CM(IRL,ICS+ICL),ICL=0,ICLMAX-1)
        WRITE(IPCNTL,4000)irl,(CM(IRL,ICS+ICL),ICL=0,ICLMAX-1)
  300 CONTINUE
C
 1000 FORMAT(/1X,' DIMENSIONS :        ',I4,' ROWS ',I4,' COLUMNS')
 2000 FORMAT(/1X,' USED :              ',I4,' ROWS ',I4,' COLUMNS')
 3000 FORMAT(/1X,' PRINT STARTS AT ROW ',I4,' COLUMN ',I4//)
c4000 FORMAT(1X,14G10.2)
c4000 FORMAT(1X,50f6.2)
!4000 FORMAT(I3,1X,20f8.4)
*4000 FORMAT(I3,1X,61f5.1)
*5000 FORMAT(4X,61I5)
c4000 FORMAT(I3,1X,61f8.4)
c5000 FORMAT(4X,61I8)
c4000 FORMAT(I3,1X,61f7.3)
c5000 FORMAT(4X,61I7)
 4000 FORMAT(I3,1X,61f6.2)
 5000 FORMAT(4X,61I6)
c-----
c     .. end of subroutine ..
c-----
c-----I----------------------------------------------------------------I
c-----
      RETURN
      END
