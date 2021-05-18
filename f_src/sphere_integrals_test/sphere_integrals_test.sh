#! /bin/bash
#
gfortran -c -Wall sphere_integrals_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sphere_integrals_test.o $HOME/lib/sphere_integrals.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sphere_integrals_test.o
#
mv a.out sphere_integrals_test
./sphere_integrals_test > sphere_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sphere_integrals_test
#
echo "Normal end of execution."
