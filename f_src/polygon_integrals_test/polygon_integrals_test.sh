#! /bin/bash
#
gfortran -c -Wall polygon_integrals_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran polygon_integrals_test.o $HOME/lib/polygon_integrals.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm polygon_integrals_test.o
#
mv a.out polygon_integrals_test
./polygon_integrals_test > polygon_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm polygon_integrals_test
#
echo "Normal end of execution."
