# Vl Numerische Mathermatik
# Prof.Dr. W. Piepke, 04.09.09
# INTEL FORTRAN90 Compiler :
#CF=/opt/intel_fc_80/bin/ifc
#FFLAGS=-g -C -u                    # debugging options
#FFLAGS=-O3 -xW -ipo -ip -align     # no debugging options
#FFLAGS=-O3 -xW -ipo -ip -align -r8 # no debugging options
# GNU FORTRAN77 Compiler :
#CF=g77
#FFLAGS=-g -Wall
# GNU FORTRAN90 Compiler :
 CF=gfortran
#                vor
#                unbenutzten
#                Variablen
#                warnen   nicht initiali= 
#                 |       sierte Varia=
#                 |       blen melden
#                 |         |
#                 V         V
#FFLAGS=  -g -O  -Wunused -Wuninitialized -fbounds-check
#FFLAGS=     -O3
#
#       Symbol= 
#       tabelle alle      Array    Prepro=     Prepro=           Prepro=       Compiler-
#         zum   Warnungen Bound    zessor      zessor            zessor        Schalter
#      Debuggen |         checking Direktive:  Direktive:        Direktive:    single -->
#          |    |         |        F90         Printout  |       Alternative   double
#          |    |         |        ifdef       Level 1  Level 2  LU oder       exe-Version
#          |    |         |         |          ifdef     |       Gauss-Seidel    |
#          |    |         |         |           |        |          |            |
#          V    V         V         V           V        V          V            V
#FFLAGS=  -g -Wall -fbounds-check
#FFLAGSPP=-g -Wall -fbounds-check -Dfortran90 -Dplevel1           -DGaussSeidel
#FFLAGSPP=-g -Wall -fbounds-check -Dfortran90 -Dplevel1 -Dplevel2 -DGaussSeidel
#FFLAGS=  -g -Wall -O1   
#FFLAGSPP=-g -Wall -O1            -Dfortran90 -Dplevel1           -DGaussSeidel
#FFLAGSPP=-g -Wall -O1            -Dfortran90 -Dplevel1           -DLUZerlegung
#FFLAGS=  -g -Wall -O2
#FFLAGSPP=-g -Wall -O2            -Dfortran90 -Dplevel1           -DGaussSeidel
#
# Das Programm ist schon in DOUBLE geschrieben : -fdefault-real-8 bewirkt nichts
#
 FFLAGS=  -g -Wall                                                              -fdefault-real-8
# FFLAGSPP=-g -Wall                -Dfortran90 -Dplevel1           -DGaussSeidel -fdefault-real-8
 FFLAGSPP=-g -Wall                -Dfortran90                     -DGaussSeidel -fdefault-real-8
# FFLAGSPP=-g -Wall                -Dfortran90 -Dplevel1           -DJacobi      -fdefault-real-8
# FFLAGSPP=-g -Wall -fbounds-check -Dfortran90                     -DJacobi      -fdefault-real-8
# FFLAGSPP=-g -Wall -fbounds-check -Dfortran90                     -DJacobi      -fdefault-real-8
#FFLAGSPP=-g -Wall                -Dfortran90 -Dplevel1           -DLUZerlegung -fdefault-real-8
#FFLAGS=  -g -Wall                                                              -fdefault-real-8
#FFLAGSPP=-g -Wall                -Dfortran90 -Dplevel1           -DConjGrad    -fdefault-real-8
#FFLAGSPP=-g -Wall -fbounds-check -Dfortran90 -Dplevel1           -DLUZerlegung
#FFLAGSPP=-g -Wall -fbounds-check -Dfortran90 -Dplevel1 -Dplevel2 -DGaussSeidel -fdefault-real-8
#FFLAGSPP=-g -Wall -fbounds-check -Dfortran90 -Dplevel1 -Dplevel2
#FFLAGS=  -g -Wall -O3
#FFLAGSPP=-g -Wall -O3            -Dfortran90 -Dplevel1           -DGaussSeidel
#
fn=FEM+Diri_ok
#
SRC=${fn}.F fem_input.F fem_randbed.F gs_solve.F mg.f printr.f dsecnd.F
OBJ=${fn}.o fem_input.o fem_randbed.o gs_solve.o mg.o printr.o dsecnd.o
# SRC=${fn}.F fem_input.F fem_randbed.F jac_solve.F mg.f printr.f dsecnd.F
# OBJ=${fn}.o fem_input.o fem_randbed.o jac_solve.o mg.o printr.o dsecnd.o
#SRC=${fn}.F fem_input.F fem_randbed.F lu_solve.F mg.f printr.f dsecnd.F
#OBJ=${fn}.o fem_input.o fem_randbed.o lu_solve.o mg.o printr.o dsecnd.o
#SRC=${fn}.F fem_input.F fem_randbed.F cg_solve.F mg.f printr.f dsecnd.F
#OBJ=${fn}.o fem_input.o fem_randbed.o cg_solve.o mg.o printr.o dsecnd.o
#


#	$(CF) ${FFLAGS} -o ${fn}.exe $(OBJ)
#	$(CF) ${FFLAGS} -o ${fn}.exe $(OBJ) /vorlesungen/piepke/lapack-3.2/lapack_LINUX.a /vorlesungen/piepke/lapack-3.2/blas_LINUX.a 
#	$(CF) ${FFLAGS} -o ${fn}.exe $(OBJ)
#	$(CF) ${FFLAGS} -o ${fn}.exe $(OBJ) /home/wolf/LAPACK/lapack_LINUX.a /home/wolf/LAPACK/blas_LINUX.a 
${fn}.exe:	$(OBJ)
	@echo " Mk.f : Try to link the executable !"
#	$(CF) ${FFLAGS} -o ${fn}.exe $(OBJ) /usr/lib/liblapack.a /usr/lib/libblas.a
	$(CF) ${FFLAGS} -o ${fn}.exe $(OBJ)
	@echo " Mk.f : the executable is linked !"

clean:
	rm $(OBJ) ${fn}.exe

.f.o :
	@echo "$< wird compiliert --> Objectdatei :"
	$(CF) ${FFLAGS} -c $<

.F.o :
	@echo "$< wird compiliert --> Objectdatei :"
	$(CF) ${FFLAGSPP} -c $<
