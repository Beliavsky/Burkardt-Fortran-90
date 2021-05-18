#! /bin/bash
#
gfortran -c -Wall machine_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran machine_test.o $HOME/lib/machine.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm machine_test.o
#
mv a.out machine_test
./machine_test > machine_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm machine_test
#
echo "Normal end of execution."
