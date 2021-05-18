#! /bin/bash
#
gfortran -c -Wall stokes_2d_exact_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o stokes_2d_exact_test stokes_2d_exact_test.o $HOME/lib/stokes_2d_exact.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm stokes_2d_exact_test.o
#
./stokes_2d_exact_test > stokes_2d_exact_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm stokes_2d_exact_test
#
gnuplot < stokes1_commands.txt
gnuplot < stokes2_commands.txt
gnuplot < stokes3_commands.txt
#
echo "Normal end of execution."
