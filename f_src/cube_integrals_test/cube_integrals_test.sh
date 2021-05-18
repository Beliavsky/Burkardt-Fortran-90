#! /bin/bash
#
gfortran -c -Wall cube_integrals_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cube_integrals_test cube_integrals_test.o $HOME/lib/cube_integrals.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cube_integrals_test.o
#
./cube_integrals_test > cube_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cube_integrals_test
#
echo "Normal end of execution."
