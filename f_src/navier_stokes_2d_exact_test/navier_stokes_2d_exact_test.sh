#! /bin/bash
#
gfortran -c -Wall navier_stokes_2d_exact_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o navier_stokes_2d_exact_test navier_stokes_2d_exact_test.o \
  $HOME/lib/navier_stokes_2d_exact.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm navier_stokes_2d_exact_test.o
#
./navier_stokes_2d_exact_test > navier_stokes_2d_exact_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm navier_stokes_2d_exact_test
#
gnuplot < gms_commands.txt
gnuplot < lukas_commands.txt
gnuplot < poiseuille_commands.txt
gnuplot < spiral_commands.txt
gnuplot < taylor_commands.txt
gnuplot < vortex_commands.txt
#
echo "Normal end of execution."
