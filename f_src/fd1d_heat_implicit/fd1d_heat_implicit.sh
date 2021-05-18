#! /bin/bash
#
gfortran -c -Wall fd1d_heat_implicit.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fd1d_heat_implicit.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fd1d_heat_implicit.o
#
mv a.out ~/bin/fd1d_heat_implicit
#
echo "Normal end of execution."
