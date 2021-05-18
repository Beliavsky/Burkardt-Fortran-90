#! /bin/bash
#
gfortran -c -Wall fd2d_heat_steady_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fd2d_heat_steady_test.o $HOME/lib/fd2d_heat_steady.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fd2d_heat_steady_test.o
#
mv a.out fd2d_heat_steady_test
./fd2d_heat_steady_test > fd2d_heat_steady_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fd2d_heat_steady_test
#
gnuplot < test01_commands.txt
#
echo "Normal end of execution."
