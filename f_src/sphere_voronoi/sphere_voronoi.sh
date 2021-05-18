#! /bin/bash
#
gfortran -c -Wall sphere_voronoi.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sphere_voronoi.o $HOME/lib/stripack.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sphere_voronoi.o
#
mv a.out ~/bin/sphere_voronoi
#
echo "Normal end of execution."
