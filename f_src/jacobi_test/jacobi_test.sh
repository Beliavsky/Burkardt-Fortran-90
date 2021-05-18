#! /bin/bash
#
gfortran -c -Wall jacobi_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran jacobi_test.o $HOME/lib/jacobi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm jacobi_test.o
#
mv a.out jacobi_test
./jacobi_test > jacobi_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm jacobi_test
#
echo "Normal end of execution."
