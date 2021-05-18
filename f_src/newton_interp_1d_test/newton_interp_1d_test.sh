#! /bin/bash
#
gfortran -c -Wall newton_interp_1d_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o newton_interp_1d_test newton_interp_1d_test.o \
  $HOME/lib/newton_interp_1d.o $HOME/lib/test_interp.o $HOME/lib/r8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm newton_interp_1d_test.o
#
./newton_interp_1d_test > newton_interp_1d_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm newton_interp_1d_test
#
gnuplot < commands01.txt
gnuplot < commands02.txt
gnuplot < commands03.txt
gnuplot < commands04.txt
gnuplot < commands05.txt
gnuplot < commands06.txt
gnuplot < commands07.txt
gnuplot < commands08.txt
#
echo "Normal end of execution."
