      program FEM2D
!-----
!-----|--1---------2---------3---------4---------5---------6---------7--|
!-----
      implicit none
!..global variables..
!..local  variables..
!..local  integer variables..
      integer pdim
      integer edim
      integer bdim
!..global arrays..
!..global integer arrays..
#ifdef LUZerlegung
      integer, dimension(:)   , allocatable :: ipiv
#endif
      integer, dimension(:,:) , allocatable :: e                       ! Knotennummern <--> Element
      integer, dimension(:,:) , allocatable :: be                      ! Knotennummern <--> Randelement
!..global real arrays..
      real   , dimension(:)   , allocatable :: b                       ! rechte Seite
      real   , dimension(:)   , allocatable :: ditb                    ! rechte Seite+Optimierung
      real   , dimension(:)   , allocatable :: blokal                  ! lokale rechte Seite
      real   , dimension(:)   , allocatable :: xneu                    ! Jacobi-Alg
      real   , dimension(:)   , allocatable :: u                       ! Loesung
      real   , dimension(:)   , allocatable :: filter                  ! Loesung
      real   , dimension(:,:) , allocatable :: p                       ! FEM-Knotenpunkte
      real   , dimension(:,:) , allocatable :: A                       ! globale Steifigkeitsmatrix
      real   , dimension(:,:) , allocatable :: ditlpu                  ! globale Steifigkeitsmatrix+Optimierung
      real   , dimension(:,:) , allocatable :: stmp                    ! temporaere Matrix
!..functions..
      integer :: countEntries
      character(len=128) :: path
!-----
!-----|--1---------2---------3---------4---------5---------6---------7--|
!-----
!
!     Es werden Dreieckselemente in dieses Rechteck gelegt.
!
!     Dimensionierung der Felder in Abhaengigkeit von den Eingaben
!     
      print*, '========================================================'
      print*, ''
      path = 'mesh.nodes'
      pdim = countEntries(path)
      path = 'mesh.elements'
      edim = countEntries(path)
      path = 'mesh.boundary'
      bdim = countEntries(path)
      print*, ''
      print*,' FEM : pdim         = ',pdim

      allocate ( p(pdim, 3) )
      allocate ( A(pdim, pdim) )
      allocate ( ditlpu(pdim, pdim) )
      allocate ( b(pdim) )
      allocate ( blokal(4) )
      allocate ( ditb(pdim) )
      allocate ( filter(pdim) )
#ifdef LUZerlegung
      allocate ( ipiv(pdim) )
#endif
      allocate ( u(pdim) )
      allocate ( xneu(pdim) )
      allocate ( e(edim, 4) )
      allocate ( be(bdim, 3) )
      allocate ( stmp(4, 4) )
!-----
!-----|--1---------2---------3---------4---------5---------6---------7--|
!-----
!
      print*,'Dimensionierung, Allokierung ----> erfolgreich'
      call fem_input(pdim, edim, bdim, p, e, be, stmp, A, b, 
     &                 blokal, filter)
!
!     Gleichungssystem ist jetzt ohne Randbedigungen aufgestellt. 
!     Es kann noch nicht geloest werden.
!
!-----
!-----|--1---------2---------3---------4---------5---------6---------7--|
!-----
!    
      print*, 'Einlesen, Filtern -------------> erfolgreich'
      call fem_randbed(pdim, bdim, b, A, p, filter)
!
!-----
!-----|--1---------2---------3---------4---------5---------6---------7--|
!-----
!
!     Gleichungssystem ist jetzt aufgestellt. Es kann geloest werden.
!
      print*, 'Randbedingung einsetzen --------> erfolgreich'
#ifdef Jacobi
      call jac_solve(pdim,
     &              a, b, ditlpu, ditb, u)
#endif


#ifdef GaussSeidel
      call gs_solve(pdim, a, b, p, ditlpu, ditb, u)
#endif
#ifdef LUZerlegung
      call lu_solve(anz_stuetzst, anz_stuetzst, pdim, 
     &              a, b, ipiv, u, h )
#endif
#ifdef ConjGrad
      call cg_solve(anz_stuetzst, anz_stuetzst, pdim, 
     &              a, b, u, h )
#endif
!
!     Speicherplatz wieder ans Betriebssystem zurueckgeben :
!
      deallocate ( p )
      deallocate ( e )
      deallocate ( A )
      deallocate ( ditlpu )
      deallocate ( b )
      deallocate ( ditb )
#ifdef LUZerlegung
      deallocate ( ipiv )
#endif
      deallocate ( u )
      deallocate ( blokal )
      deallocate ( stmp )

      print*, '========================================================'
      print*, ''

      stop 
      end

        function countEntries(strng) result(n)

        implicit none
        character(len=128) :: strng
        integer(kind=4) :: i, ios, n
        real(kind=4) :: dummy

        write(unit = 6, fmt = *) 'countEntries: ', strng
        i = 1
        ios = 0
        open(unit = 33, iostat = ios, file = strng, status = 'old') 
        if (ios .ne. 0) then
            write(unit = 6, fmt= *)'countEntries: Fehler beim Dateipfad'
        else
            do while (ios .eq. 0)
                read(unit = 33, fmt = *, iostat = ios) dummy
                i = i + 1
            end do
            n = i - 2
            write(unit = 6, fmt = *) 'Anzahl der Einträge: ', n
            write(unit = 6, fmt = *) '--------'
        end if
        close(unit = 33, status = 'keep')

        end function countEntries
