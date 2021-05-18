#! /bin/bash
#
gfortran -c -Wall fd1d_advection_ftcs.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fd1d_advection_ftcs.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm fd1d_advection_ftcs.o
mv a.out ~/bin/fd1d_advection_ftcs
#
echo "Normal end of execution."
