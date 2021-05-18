#! /bin/bash
#
gfortran -c -Wall r8poly_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o r8poly_test r8poly_test.o $HOME/lib/r8poly.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm r8poly_test.o
#
./r8poly_test > r8poly_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm r8poly_test
#
echo "Normal end of execution."
