#! /bin/bash
#
gfortran -c -Wall hyperball_integrals_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran hyperball_integrals_test.o $HOME/lib/hyperball_integrals.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hyperball_integrals_test.o
#
mv a.out hyperball_integrals_test
./hyperball_integrals_test > hyperball_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hyperball_integrals_test
#
echo "Normal end of execution."
