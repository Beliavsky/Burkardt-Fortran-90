#! /bin/bash
#
gfortran -c -Wall jacobi_eigenvalue_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran jacobi_eigenvalue_test.o $HOME/lib/jacobi_eigenvalue.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm jacobi_eigenvalue_test.o
#
mv a.out jacobi_eigenvalue_test
./jacobi_eigenvalue_test > jacobi_eigenvalue_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm jacobi_eigenvalue_test
#
echo "Normal end of execution."
