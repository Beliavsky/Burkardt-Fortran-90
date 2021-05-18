#! /bin/bash
#
gfortran -c -Wall luhn_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran luhn_test.o $HOME/lib/luhn.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm luhn_test.o
#
mv a.out luhn_test
./luhn_test > luhn_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm luhn_test
#
echo "Normal end of execution."
