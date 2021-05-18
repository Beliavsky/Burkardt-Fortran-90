#! /bin/bash
#
gfortran -c -Wall ode_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran ode_test.o $HOME/lib/ode.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ode_test.o
#
mv a.out ode_test
./ode_test > ode_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ode_test
#
echo "Normal end of execution."
