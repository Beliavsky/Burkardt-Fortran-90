#! /bin/bash
#
gfortran -c -Wall naca_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran naca_test.o $HOME/lib/naca.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm naca_test.o
#
mv a.out naca_test
./naca_test > naca_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm naca_test
#
echo "Normal end of execution."
