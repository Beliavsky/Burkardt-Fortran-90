#! /bin/bash
#
gfortran -c -Wall sphere_cvt.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sphere_cvt.o $HOME/lib/stripack.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sphere_cvt.o
#
mv a.out ~/bin/sphere_cvt
#
echo "Normal end of execution."
