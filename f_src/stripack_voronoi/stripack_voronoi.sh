#! /bin/bash
#
gfortran -c -Wall stripack_voronoi.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran stripack_voronoi.o $HOME/lib/stripack.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm stripack_voronoi.o
#
chmod ugo+x a.out
mv a.out $HOME/bin/stripack_voronoi
#
echo "Normal end of execution."
