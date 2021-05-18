#! /bin/bash
#
gfortran -c -Wall predator_plot3d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran predator_plot3d.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm predator_plot3d.o
#
mv a.out predator_plot3d
./predator_plot3d > predator_plot3d.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm predator_plot3d
#
#  Create graphics.
#
gnuplot < predator_commands.txt
#
echo "Normal end of execution."
