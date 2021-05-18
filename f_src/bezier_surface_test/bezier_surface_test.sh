#! /bin/bash
#
gfortran -c -Wall bezier_surface_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o bezier_surface_test bezier_surface_test.o $HOME/lib/bezier_surface.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm bezier_surface_test.o
#
./bezier_surface_test > bezier_surface_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm bezier_surface_test
#
echo "Normal end of execution."
