#! /bin/bash
#
gfortran -c -Wall sphere_delaunay.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sphere_delaunay.o $HOME/lib/stripack.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sphere_delaunay.o
#
mv a.out ~/bin/sphere_delaunay
#
echo "Normal end of execution."
