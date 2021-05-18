#! /bin/bash
#
gfortran -c -Wall monomial_value_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o monomial_value_test monomial_value_test.o $HOME/lib/monomial_value.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm monomial_value_test.o
#
./monomial_value_test > monomial_value_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm monomial_value_test
#
echo "Normal end of execution."
