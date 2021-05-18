#! /bin/bash
#
gfortran -c -Wall monomial_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran monomial_test.o $HOME/lib/monomial.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm monomial_test.o
#
mv a.out monomial_test
./monomial_test > monomial_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm monomial_test
#
echo "Normal end of execution."
