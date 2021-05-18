#! /bin/bash
#
gfortran -c -Wall fem2d_sample.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fem2d_sample.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm fem2d_sample.o
#
chmod ugo+x a.out
mv a.out ~/bin/fem2d_sample
#
echo "Normal end of execution."
