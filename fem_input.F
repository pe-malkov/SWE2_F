      subroutine fem_input(pdim, edim, bdim, p, e, be, stmp, A, b, 
     &                 blokal, filter)
c-----
c-----|--1---------2---------3---------4---------5---------6---------7--|
c-----
      implicit none
c..global variables..
c..global integer variables..
      integer          pdim
      integer          edim
      integer          bdim
c..global real variables..
C..global arrays..
C..global integer arrays..
      integer          e(edim, 4)
      integer          be(bdim, 3)
C..global real arrays..
      real             b(pdim)
      real             filter(pdim)
      real             blokal(4)
      real             p(pdim, 3)
      real             A(pdim, pdim)
      real             stmp(4, 4)
c..local  variables..
c..local  integer variables..
      integer          ISTRG, ios
      integer          i, j, l, r, ir, jr, node, dummyInt
      integer          ei
      integer          maxprint
c..local  character variables..
      character(len = 16) STRG
c..local  real variables..
      real             dummyReal
      real          :: xmax, ymax, zmax
      real          :: xmin, ymin, zmin
      real(kind=8)  :: x1, x2, x3, x4 
      real(kind=8)  :: y1, y2, y3, y4 
      real(kind=8)  :: z1, z2, z3, z4 
      real(kind=8)  :: a1, b1, c1, d1, e1, f1, g1, h1, i1 
      real(kind=8)  :: phi11, phi12, phi13 
      real(kind=8)  :: phi21, phi22, phi23 
      real(kind=8)  :: phi31, phi32, phi33 
      real(kind=8)  :: a2, b2, c2, d2, e2, f2 
      real(kind=8)  :: det_J 
c-----
c-----|--1---------2---------3---------4---------5---------6---------7--|
c-----

#ifdef plevel2
      print*,' fem_input : pdim   = ',pdim
      print*,' fem_input : edim   = ',edim
      print*,' fem_input : bdim   = ',bdim
#endif
!-------WRITE NODES------------------------------------------------------------------------

        ios = 0
        open(unit = 33, iostat = ios, file = 'mesh.nodes')
        open(unit = 34, file = 'nodes.dem')
        do ir = 1, pdim 
            filter(ir) = ir
            read(unit = 33, fmt = *, iostat = ios) dummyInt, dummyReal,
     &   p(ir, 1), p(ir, 2), p(ir, 3) 
            write(unit = 34, fmt = *) p(ir, 1), p(ir, 2), 
     &       p(ir, 3)
        end do
        close(unit = 33, status = 'keep')
        close(unit = 34, status = 'keep')

!------WRITE ELEMENT NODES----------------------------------------------------------------

        open(unit = 33, iostat = ios, file = 'mesh.elements', 
     &                   status = 'old') 
        open(unit = 34, file = 'elements')
        if (ios .ne. 0) then
            write(unit = 6, fmt = *) 'Fehler beim Dateipfad'
        else
            do ir = 1, edim 
                read(unit = 33, fmt = *, iostat = ios) dummyInt, 
     &   dummyInt, dummyInt, 
     &   e(ir, 1), e(ir, 2), e(ir, 3), e(ir, 4) 
                write(unit = 34, fmt = *) e(ir, 1),  e(ir, 2), 
     &           e(ir, 3), e(ir, 4)
            end do
        end if
        close(unit = 33, status = 'keep')
        close(unit = 34, status = 'keep')

        open(unit = 34, file = 'elements.dem')
        do ir = 1, edim
            do jr = 1, 4
                node = e(ir, jr) 
                write(unit = 34, fmt = *) node, p(node, 1), 
     &          p(node, 2), p(node, 3)
            end do
        end do
        close(unit = 34, status = 'keep')

!------WRITE BOUNDARY NODES-----------------------------------------------------------------

        xmax = -10000.0d+00
        xmin =  10000.0d+00
        ymax = -10000.0d+00
        ymin =  10000.0d+00
        zmax = -10000.0d+00
        zmin =  10000.0d+00
        open(unit = 33, file = 'minmax')
        do i = 1, pdim
           xmax = max(xmax, p(i, 1))
           xmin = min(xmin, p(i, 1))
           ymax = max(ymax, p(i, 2))
           ymin = min(ymin, p(i, 2))
           zmax = max(zmax, p(i, 3))
           zmin = min(zmin, p(i, 3))
        end do
        write(unit =33, fmt= *) 'max xyz: ', xmax, ymax, zmax
        write(unit =33, fmt= *) 'min xyz: ', xmin, ymin, zmin
        close(unit =33, status = 'keep')

        open(unit = 35, iostat = ios, file = 'mesh.boundary', 
     &                  status = 'old') 
        open(unit = 36, file = 'boundary')
        if (ios .ne. 0) then
            write(unit = 6, fmt = *) 'Fehler beim Dateipfad'
        else
            do ir = 1,bdim 
                read(unit = 35, fmt = *, iostat = ios) dummyInt,
     &   dummyInt, dummyInt, dummyInt, dummyInt,
     &              be(ir, 1), be(ir, 2), be(ir, 3)
                write(unit = 36, fmt = *) be(ir, 1),  be(ir, 2),
     &           be(ir, 3)
            end do
        end if
        close(unit = 35, status = 'keep')
        close(unit = 36, status = 'keep')

        open(unit = 37, file = 'filter')
        open(unit = 38, file = 'boundary.dem')
        do ir = 1, bdim
          do jr = 1, 3
            node = be(ir, jr) 
              if (
     &           ((p(node, 1) .gt. xmin) .and. (p(node, 1) .lt. xmax))
     &      .and. ((p(node, 2) .gt. ymin) .and. (p(node, 2) .lt. ymax))
     &      .and. ((p(node, 3) .gt. zmin) .and. (p(node, 3) .lt. zmax))
     &           )
     &        then
                cycle
              else
               if (filter(node) .lt. 0) then
                 cycle
               else
                 filter(node) = -filter(node)
               end if
              end if
                write(unit = 38, fmt = *) p(node, 1), 
     &           p(node, 2), p(node, 3)
          end do
        end do



        close(unit = 37, status = 'keep')
        close(unit = 38, status = 'keep')


        open(unit = 33, file = 'filter')
        do i = 1, pdim
           write(unit = 33, fmt = *) filter(i)     
        end do
        close(unit = 33, status = 'keep')
!==============================================================================        
#ifdef plevel1
      print*,' Element - Knotennummern  :'
      do ei = 1, edim
         write( unit = 6, fmt = 2000 ) ei, e( ei, 1 ), 
     &                                     e( ei, 2 ), 
     &                                     e( ei, 3 ),
     &                                     e( ei, 4 )
      end do
#endif
c
      b  = 0.0
      stmp = 0.0
      A = 0.0
c
      do ei = 1, edim
! Hier sonst fuer jedes Element separat die Berechnung der lokalen Steifigkeitsmatrix(Jacobi_matrix)
!                                                                                   Jacobi_matrix(Knotenkoordinaten)
      x1 = p(e(ei, 1), 1)
      y1 = p(e(ei, 1), 2)
      z1 = p(e(ei, 1), 3)

      x2 = p(e(ei, 2), 1)
      y2 = p(e(ei, 2), 2)
      z2 = p(e(ei, 2), 3)

      x3 = p(e(ei, 3), 1)
      y3 = p(e(ei, 3), 2)
      z3 = p(e(ei, 3), 3)

      x4 = p(e(ei, 4), 1)
      y4 = p(e(ei, 4), 2)
      z4 = p(e(ei, 4), 3)
  
      a1 = x2 - x1 !r
      b1 = y2 - y1
      c1 = z2 - z1
      d1 = x3 - x1
      e1 = y3 - y1
      f1 = z3 - z1
      g1 = x4 - x1
      h1 = y4 - y1
      i1 = z4 - z1

!tranp.
!      a1 = x2 - x1
!      b1 = x3 - x1
!      c1 = x4 - x1
!      d1 = y2 - y1
!      e1 = y3 - y1
!      f1 = y4 - y1
!      g1 = z2 - z1
!      h1 = z3 - z1
!      i1 = z4 - z1

      det_J = a1*e1*i1 + b1*f1*g1 + c1*d1*h1 -  g1*e1*c1 - h1*f1*a1
     &                                                   - i1*d1*b1  

      phi11 = e1*i1 - f1*h1 !r
      phi12 = f1*g1 - d1*i1
      phi13 = d1*h1 - e1*g1
      phi21 = c1*h1 - b1*i1
      phi22 = a1*i1 - c1*g1
      phi23 = b1*g1 - a1*h1
      phi31 = b1*f1 - c1*e1
      phi32 = c1*d1 - a1*f1
      phi33 = a1*e1 - b1*d1

!transp.      
!      phi11 = e1*i1 - f1*h1
!      phi21 = f1*g1 - d1*i1
!      phi31 = d1*h1 - e1*g1
!      phi12 = c1*h1 - b1*i1
!      phi22 = a1*i1 - c1*g1
!      phi32 = b1*g1 - a1*h1
!      phi13 = b1*f1 - c1*e1
!      phi23 = c1*d1 - a1*f1
!      phi33 = a1*e1 - b1*d1

!      a2 = phi11**2 + phi21**2 + phi31**2 !r
!      b2 = phi12**2 + phi22**2 + phi32**2
!      c2 = phi13**2 + phi23**2 + phi33**2
!      d2 = phi11*phi12 + phi21*phi22 + phi31*phi32
!      e2 = phi11*phi13 + phi21*phi23 + phi31*phi33
!      f2 = phi12*phi13 + phi22*phi23 + phi32*phi33

!transp.
      a2 = phi11**2 + phi12**2 + phi13**2
      b2 = phi21**2 + phi22**2 + phi23**2
      c2 = phi31**2 + phi32**2 + phi33**2
      d2 = phi11*phi21 + phi12*phi22 + phi13*phi23
      e2 = phi11*phi31 + phi12*phi32 + phi13*phi33
      f2 = phi21*phi31 + phi22*phi32 + phi23*phi33

      stmp(1,1) = a2 + b2 +c2 +2*d2 + 2*e2 + 2*f2
      stmp(1,2) = -a2 - d2 - e2
      stmp(1,3) = -b2 - d2 - f2
      stmp(1,4) = -c2 - e2 - f2
      stmp(2,1) = -a2 - d2 - e2
      stmp(2,2) = a2
      stmp(2,3) = d2
      stmp(2,4) = e2
      stmp(3,1) = -b2 - d2 - f2
      stmp(3,2) = d2
      stmp(3,3) = b2
      stmp(3,4) = f2
      stmp(4,1) = -c2 - e2 - f2
      stmp(4,2) = e2
      stmp(4,3) = f2
      stmp(4,4) = c2

      blokal = det_J / 6.0
      stmp = stmp / (det_J*6.0)

         do i = 1, 4                                                   ! 4 wg.Tetraederelement
            l = e(ei, i)
            do j = 1, i
               r = e(ei, j)
               A(l, r) = A(l, r) + stmp(i,j)
               A(r, l) = A(l, r)
            end do
            b(l) = b(l) + blokal(i)
         end do
      end do
c
      ISTRG = 16
      STRG  = 'Steif.Mat.ohneRB'
      maxprint = 15
#ifdef plevel1
      CALL PRINTR(ISTRG, STRG, A, pdim, pdim, maxprint, maxprint,
     >            1, 1, maxprint, maxprint)
c
      print*, ' Rechte Seite ohne Randbedingungen :'
      do l = 1, pdim
         write( unit= 6, fmt=3000) l, b(l)
      end do
 3000 format(' b(',i4,') = ',g12.4)
#endif
      open(unit = 33, file = 'Sb-RAND')
      print*, 'Sb-rand'
      do i = 1, pdim
         do j = 1, pdim
            write(unit = 33, fmt = *) i, j, A(i, j)
         end do
      end do
      close(unit = 33, status = 'keep')
!      CALL PRINTR(ISTRG, STRG, A, pdim, pdim, maxprint, maxprint,
!     >            1, 1, maxprint, maxprint)
c
      print*, ' Rechte Seite ohne Randbedingungen :'
      do l = 1, pdim
         write( unit= 6, fmt=3000) l, b(l)
      end do
 3000 format(' b(',i4,') = ',g12.4)
#ifdef plevel1
 2000 format(' k = ',i4,' P1 k = ',i4,' P2 k = ',i4,' P3 k = ',i4)
#endif
c-----
c-----|--1---------2---------3---------4---------5---------6---------7--|
c-----
      return
      end
