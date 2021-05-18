#! /bin/bash
#
gfortran -c -Wall gfortran_intrinsics_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran gfortran_intrinsics_test.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm gfortran_intrinsics_test.o
#
mv a.out gfortran_intrinsics_test
./gfortran_intrinsics_test > gfortran_intrinsics_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm gfortran_intrinsics_test
#
echo "Normal end of execution."
