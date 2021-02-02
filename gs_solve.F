!-----
!-----|--1---------2---------3---------4---------5---------6---------7--|
!-----
!     Unterprogrammname :
      subroutine gs_solve(ndim, a, b, p, ditlpu, ditb, x)
!-----
!-----|--1---------2---------3---------4---------5---------6---------7--|
!-----
!     Vl Numerische Mathermatik
!     Prof.Dr. W. Piepke, 04.09.2009
!-----|--1---------2---------3---------4---------5---------6---------7--|
!
!     1. Versuch : Programm f"ur den iterativen Gauss--Seidelalgorithmus
!     -->          Determinante ungleich 0 wird nicht ueberprueft !
!     -->          Dies ist kein Algoritmus fuer schlecht 
!                  konditionierte (ill-conditioned) Gleichungssysteme.
!     -->          ...
!
!-----|--1---------2---------3---------4---------5---------6---------7--|
!     Deklarationen :
!-----|--1---------2---------3---------4---------5---------6---------7--|
      implicit none

!..global variables..

!..global integer variables..
      integer    ndim
!..global real variables..
      real       h
      real       u     !Lösungswert zum Vergleich mit gerechneter Lösung
!..global arrays..

!..global real arrays..
      real       x(ndim), b(ndim), ditb(ndim), p(ndim,3)
      real       a(ndim, ndim), ditlpu(ndim, ndim)

!..local  variables..

!..local character variables..
      character(len=16) strg
!..local  integer variables..
      integer    istrg
      integer    it_max
      parameter (it_max = 8000)
      integer    i
      integer    it
      integer    j
!      integer    k
      integer    maxprint
!..local  real variables..
      real       delta     ! Abweichung(Fehler) von x(i)-u(i)
      real       eps
      real       proof
      real       proof_abs
      real       proof_max
      real       proof_l2sq
      real       slamch
      real       temp
      real       w
!-----|--1---------2---------3---------4---------5---------6---------7--|
!     Fuer die Zeitmessung
!     integer           mclock
      real              cpu_sekunden, st0, ste
      DOUBLE PRECISION   DSECND
      EXTERNAL           DSECND
!-----|--1---------2---------3---------4---------5---------6---------7--|
      external input
      external output
      external slamch
!-----|--1---------2---------3---------4---------5---------6---------7--|
!     Anweisungen :
!-----|--1---------2---------3---------4---------5---------6---------7--|
!
      w = 1.2
      STRG='                '
!
#ifdef plevel1
      print*,' gs_solve : ndim  = ',ndim
#endif
      eps = slamch('e')
      eps = eps * 0.000000001             ! 33 Stuetzstellen
!     eps = eps * 0.0000000001            ! 17 Stuetzstellen
!     eps = 0.001
*     eps = 0.0001
!     eps = 0.00001
!      eps = 0.000001
!      eps = 0.0000000001
#ifdef plevel1
      print*,' gs_solve : used averaged relative machine precision ',
     1         ' epsilon eps      = ',eps
#endif
!     Algorithmusanfang :
      st0 = dsecnd()
!     Vorgabe des Anfangswertes :
      x = 0.0
      ditlpu = 0.0
      ditb   = 0.0
!     Berechnen der Inv(D)x(L + U)-Matrix :
      do j = 1, ndim
         do i = 1, ndim
!           Schreiben der (L + U)-Matrix :
!           (Division erfolgt einmal vor der Iterationsschleife !)
            ditlpu(j, i) = a(i, j)/a(i, i)
         end do
         ditlpu(j, j) = 0.0
!        (Division erfolgt einmal vor der Iterationsschleife !)
         ditb(j)      = b(j)/a(j, j)
      end do
      ISTRG = 12
      STRG = 'Inv(D)*(L+R)'
      if ( ndim .gt. 15 ) then
         maxprint = 15
      else
         maxprint = ndim
      end if
#ifdef plevel3
      CALL PRINTR(ISTRG, STRG, ditlpu, ndim, ndim, maxprint, maxprint,
     >            1, 1, maxprint, maxprint)
#endif
!     Iterationsschleife :
      do 100 it = 1, it_max
!        Aeuessere Schleife ueber die Komponenten des zu iterierenden
!        Vektors der Unbekannten :
!        Das Schleifennest wurde nun umstrukturiert durch
!        identische Umformungen
!        1. Vertauschen der Zeilen und Spalten in der 
!           Inv(D)x(L + U)-Matrix : ==> Spaltenweiser Datenzugriff
!           Dann laeuft die innerste Schleife ueber die Matrixspalten !
!        2. Benutzen der Matrix (L + U) ==>
!           Es wird nun zwar mit 0 gerechnet falls Diagonalelemente
!           verarbeitet werden, jedoch sind Gleitkommazahleoperationen,
!           die in die Hardware abgebildet werden koennen, schneller als
!           if--Abfragen !
         do 200 i = 1, ndim
            temp = 0.0
            do j = 1, ndim
               temp = temp + ditlpu(j, i) * x(j)
            end do
            x(i) = (1.0-w)*x(i) + w*(ditb(i) - temp)
  200    continue
!        Qualitaet der erreichten Loesung wird ueberprueft, ob das
!        Gleichungssystem Ax - b = 0 erfuellt ist, also die 0
!        auf der rechten Seite reproduziert wird.
         proof_l2sq = 0.0
         proof_abs  = 0.0
         proof_max  = 0.0
         proof = 0.0
         do 300 i = 1, ndim
            temp = 0.0
            do 400 j = 1, ndim
               temp = temp + a(i, j) * x(j)
  400       continue
!           Aufsummierung der Fehler : Diese nennt man auch Residuen
            proof_max = max(proof_max , abs(b(i) - temp))
            proof_l2sq = proof_l2sq + (b(i) - temp) * (b(i) - temp)
            proof_abs = proof_abs + abs(b(i) - temp)
            proof = proof + b(i) - temp
!
#ifdef plevel3
            print*,' it = ',it,' x(',i,') = ',x(i)
#endif
!
  300    continue
!        Mittlerer Fehler entscheidet ueber den Iterationsabbruch
!        proof  = proof / real(ndim)
!        Fehler in der euklidischen Norm :
         proof  = proof_l2sq
!
#ifdef plevel1
         write(unit=6, fmt=9900)
     &        it, proof_max, proof_l2sq, proof_abs, proof, eps
         write(unit= 44, fmt=9800)
     &        it, proof_max, proof_l2sq, proof_abs, proof, eps
#endif
!
         if (abs(proof) .le. eps) then
!        if (abs(proof_abs) .le. eps) then
!        if (abs(proof_max) .le. eps) then
!        if (abs(proof_l2sq) .le. eps) then
#ifdef plevel2
            do i = 1, ndim
               print*,' it = ',it-1,' x(',i,') = ',x(i)
            end do
            print*,' '
#endif
!           Abbruch der Iterationen, da mit einer ausreichenden
!           Genauigkeit die Loesung gefunden wurde.
            print*,' Es wurde in ',it,' Schritten eine'
            print*,' akzeptable Loesung gefunden.'
!           Ergebnisausgabe :
!           ste = real(mclock()) * 1.0/100.0
            ste = dsecnd()
            cpu_sekunden = (ste - st0)
            print*,' Dazu wurden ',cpu_sekunden,
     &             ' CPU--Sekunden benoetigt.'
            open(unit = 33, file = 'delta.dat')
            do i = 1, ndim
                u= 20.0 - 2.0*p(i,2)**2 + p(i,1)**3*p(i,2)
     &                  - p(i,1)*p(i,2)**3 + p(i,3)**3*p(i,1) 
     &                  - p(i,3)*p(i,1)**3
                delta = abs(x(i) - u)
                print*,'delta: ', i, delta
                write(unit = 33, fmt = *) i, delta
            end do
            close(unit = 33, status = 'keep')
            call system('gnuplot delta.dem')
            stop
         end if

  100 continue
!------------------------------------------------------------------------
 9000 format(5(' ',e14.6))
 9800 format(' ',i6,' ',e16.8,' ',e16.8,' ',e16.8,
     &       ' ',e16.8,' ',e16.8)
 9900 format(' cg : it = ',i6,' proof_max = ',e16.8,
     &       ' proof_l2sq = ',e16.8,' proof_abs = ',e16.8,
     &       ' proof/n = ',e16.8,' epsilon = ',e16.8)
!-----|--1---------2---------3---------4---------5---------6---------7--|
      STOP                                                             ! Programmbeendung
      END
