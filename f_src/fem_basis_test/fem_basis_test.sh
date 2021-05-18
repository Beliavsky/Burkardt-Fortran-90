#! /bin/bash
#
gfortran -c -Wall fem_basis_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fem_basis_test.o $HOME/lib/fem_basis.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fem_basis_test.o
#
mv a.out fem_basis_test
./fem_basis_test > fem_basis_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fem_basis_test
#
echo "Normal end of execution."
