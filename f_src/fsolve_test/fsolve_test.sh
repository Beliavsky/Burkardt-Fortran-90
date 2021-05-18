#! /bin/bash
#
gfortran -c -Wall fsolve_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fsolve_test.o $HOME/lib/fsolve.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fsolve_test.o
#
mv a.out fsolve_test
./fsolve_test > fsolve_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fsolve_test
#
echo "Normal end of execution."
