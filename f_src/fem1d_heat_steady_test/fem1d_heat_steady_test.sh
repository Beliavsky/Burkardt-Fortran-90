#! /bin/bash
#
gfortran -c -Wall fem1d_heat_steady_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fem1d_heat_steady_test.o $HOME/lib/fem1d_heat_steady.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fem1d_heat_steady_test.o
#
mv a.out fem1d_heat_steady_test
./fem1d_heat_steady_test > fem1d_heat_steady_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fem1d_heat_steady_test
#
echo "Normal end of execution."
