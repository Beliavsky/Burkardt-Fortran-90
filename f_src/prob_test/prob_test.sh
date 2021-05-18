#! /bin/bash
#
gfortran -c -Wall prob_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran prob_test.o $HOME/lib/prob.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm prob_test.o
#
mv a.out prob_test
./prob_test > prob_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm prob_test
#
echo "Normal end of execution."
