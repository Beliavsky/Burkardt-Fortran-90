#! /bin/bash
#
gfortran -c -Wall power_method_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran power_method_test.o $HOME/lib/power_method.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm power_method_test.o
#
mv a.out power_method_test
./power_method_test > power_method_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm power_method_test
#
echo "Normal end of execution."
