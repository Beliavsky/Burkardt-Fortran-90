#! /bin/bash
#
gfortran -c -Wall cvt_triangulation.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran cvt_triangulation.o $HOME/lib/test_triangulation.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cvt_triangulation.o
#
mv a.out $HOME/bin/cvt_triangulation
#
echo "Normal end of execution."
