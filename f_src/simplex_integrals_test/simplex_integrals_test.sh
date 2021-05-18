#! /bin/bash
#
gfortran -c -Wall simplex_integrals_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran simplex_integrals_test.o $HOME/lib/simplex_integrals.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm simplex_integrals_test.o
#
mv a.out simplex_integrals_test
./simplex_integrals_test > simplex_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm simplex_integrals_test
#
echo "Normal end of execution."
