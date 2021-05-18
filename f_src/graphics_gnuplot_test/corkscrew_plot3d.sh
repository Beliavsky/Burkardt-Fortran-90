#! /bin/bash
#
gfortran -c -Wall corkscrew_plot3d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran corkscrew_plot3d.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm corkscrew_plot3d.o
#
mv a.out corkscrew_plot3d
./corkscrew_plot3d > corkscrew_plot3d.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm corkscrew_plot3d
#
#  Create graphics.
#
gnuplot < corkscrew_commands.txt
#
echo "Normal end of execution."
