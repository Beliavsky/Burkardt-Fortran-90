#! /bin/bash
#
gfortran -c -Wall gfortran_quadmath_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran gfortran_quadmath_test.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm gfortran_quadmath_test.o
#
mv a.out gfortran_quadmath_test
./gfortran_quadmath_test > gfortran_quadmath_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm gfortran_quadmath_test
#
echo "Normal end of execution."
