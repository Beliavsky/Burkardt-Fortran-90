#! /bin/bash
#
gfortran -c -Wall disk_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o disk_grid_test disk_grid_test.o $HOME/lib/disk_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm disk_grid_test.o
#
./disk_grid_test > disk_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm disk_grid_test
#
gnuplot < disk_grid_test01_commands.txt
gnuplot < disk_grid_test02_commands.txt
#
echo "Normal end of execution."
