#! /bin/bash
#
gfortran -c -Wall stripack_delaunay.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran stripack_delaunay.o $HOME/lib/stripack.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm stripack_delaunay.o
#
chmod ugo+x a.out
mv a.out $HOME/bin/stripack_delaunay
#
echo "Normal end of execution."
