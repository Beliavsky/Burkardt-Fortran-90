#! /bin/bash
#
topdir="/home/burkardt/software_install/MUMPS_5.1.2"
INCLUDE=$topdir"/include"
#
mpif90 -c -I $INCLUDE mumps_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
LIBDMUMPS=$topdir"/lib/libdmumps.a"

LIBMUMPS_COMMON=$topdir"/lib/libmumps_common.a"

LMETISDIR="/usr/lib"
LMETIS="-L"$LMETISDIR" -lmetis"

LPORDDIR=$topdir"/PORD/lib/"
LPORD="-L"$LPORDDIR" -lpord"

LSCOTCHDIR="/usr/lib"
LSCOTCH="-L"$LSCOTCHDIR" -lesmumps -lscotch -lscotcherr"

LORDERINGS=$LMETIS" "$LPORD" "$LSCOTCH

SCALAP="-lscalapack-openmpi -lblacs-openmpi  -lblacsF77init-openmpi -lblacsCinit-openmpi"
LAPACK="-llapack"
LIBS=$SCALAP" "$LAPACK"  -lmpi -lmpi_mpifh"

LIBBLAS=-lblas

LIBOTHERS=-lpthread
#
mpif90 -o mumps_test mumps_test.o $LIBDMUMPS $LIBMUMPS_COMMON $LORDERINGS $LIBS $LIBBLAS $LIBOTHERS
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm mumps_test.o
#
mpirun -np 2 ./mumps_test < mumps_test_input.txt > mumps_test_ubuntu.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm mumps_test
#
echo "Normal end of execution."


