      subroutine fem_randbed(pdim, bdim, b, A, p, filter)
c-----
c-----|--1---------2---------3---------4---------5---------6---------7--|
c-----
c..global variables..
C..global integer variables..
      integer pdim
      integer bdim
C..global arrays..
C..global integer arrays..
      real    filter(pdim)
C..global real arrays..
      real    be(bdim)
      real    b(pdim)
      real    A(pdim, pdim)
      real    p(pdim, 3)
!      real    randwert
c..local  variables..
c..local  integer variables..
      integer ISTRG
      integer j
      integer ki
#ifdef plevel2
      integer l
      integer maxprint
      integer l
#endif
c..local  character variables..
      character(len = 16) STRG
      real    randwert
c-----
c-----|--1---------2---------3---------4---------5---------6---------7--|
c-----
!=========TEST=========
!        open(unit = 33, file = 'StiffMat-RB.out')
!        open(unit = 34, file = 'sb-rb_vgl')
!        do i = 1, 262
!           do j = 1, 262
!              read(unit = 33, fmt = *) dummyInt, dummyInt, A(j, i)
!              write(unit = 34, fmt = *) dummyInt, dummyInt, A(j, i)
!           end do
!        end do
!        close(unit = 33, status = 'keep')
!        close(unit = 34, status = 'keep')
!=========TEST=========

      do ki = 1, pdim
C     Multiplikation der Steifigkeitsmatrix mit T_0 in der k_ten Spalte
         if (filter(ki) .lt. 0) then
           do j = 1, pdim
               randwert = 20.0 - 2.0*p(ki,2)**2 + p(ki,1)**3*p(ki,2)
     &                         - p(ki,1)*p(ki,2)**3 + p(ki,3)**3*p(ki,1)
     &                         - p(ki,3)*p(ki,1)**3
               b(j)     = b(j) - A(j, ki) * randwert
               A(j, ki) = 0.0
               A(ki, j) = 0.0
           end do
         A(ki, ki) = 1.0
         end if
      end do

      do ki = 1, pdim
         if (filter(ki) .lt. 0) then
            b(ki) = 20.0 - 2.0*p(ki,2)**2 + p(ki,1)**3*p(ki,2)
     &                   - p(ki,1)*p(ki,2)**3 + p(ki,3)**3*p(ki,1)
     &                   - p(ki,3)*p(ki,1)**3 
         end if
      end do
c
!=========TEST=========
!        open(unit = 33, file = 'StiffMat+RB.out')
!        open(unit = 34, file = 'sb+rb_vgl')
!        do i = 1, 262
!           do j = 1, 262
!              read(unit = 33, fmt = *) dummyInt, dummyInt, A(j, i)
!              write(unit = 34, fmt = *) dummyInt, dummyInt, A(j, i)
!           end do
!        end do
!        close(unit = 33, status = 'keep')
!        close(unit = 34, status = 'keep')

        
!        open(unit = 33, file = 'b+RB.out')
!        do i = 1, 262
!              read(unit = 33, fmt = *) dummyInt, b(i)
!        end do
!        close(unit = 33, status = 'keep')
!=========TEST=========
      ISTRG = 16
      STRG  = 'Steif.Mat.mitRBu'
#ifdef plevel2
      CALL PRINTR(ISTRG, STRG, A, pdim, pdim, maxprint, maxprint,
     >            1, 1, maxprint, maxprint)
c
      print*, ' Rechte Seite b mit unteren Randbedingungen :'
      do l = 1, pdim
         print*, ' b+rand(',l,') = ',b(l)
!         write( unit= 6, fmt=3000) l, b(l)
      end do
#endif
      maxprint = 20
      open(unit = 33, file = 'Sb+RAND')
      do i = 1, pdim
         do j = 1, pdim
            write(unit = 33, fmt = *) i, j, A(i, j)
         end do
      end do
      close(unit = 33, status = 'keep')
!      do i = 1, pdim
!         do j = 1, pdim
!            print*, i, j, A(i, j)
!         end do
!      end do
!      CALL PRINTR(ISTRG, STRG, A, pdim, pdim, maxprint, maxprint,
!     >            1, 1, maxprint, maxprint)
      do l = 1, pdim
         print*, ' b+rand(',l,') = ',b(l)
!         write( unit= 6, fmt=3000) l, b(l)
      end do
c-----
c-----|--1---------2---------3---------4---------5---------6---------7--|
c-----
      return
      end
