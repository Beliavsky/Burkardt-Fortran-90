#! /bin/bash
#
gfortran -c -Wall triangle_integrals_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o triangle_integrals_test triangle_integrals_test.o $HOME/lib/triangle_integrals.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm triangle_integrals_test.o
#
./triangle_integrals_test > triangle_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm triangle_integrals_test
#
echo "Normal end of execution."
