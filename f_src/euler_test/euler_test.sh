#! /bin/bash
#
gfortran -c -Wall euler_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o euler_test euler_test.o $HOME/lib/euler.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm euler_test.o
#
./euler_test > euler_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm euler_test
#
echo "Generate png image."
#
gnuplot < euler_humps_commands.txt
#
echo "Normal end of execution."
