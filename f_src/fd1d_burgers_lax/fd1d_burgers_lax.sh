#! /bin/bash
#
gfortran -c -Wall fd1d_burgers_lax.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fd1d_burgers_lax.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fd1d_burgers_lax.o
#
mv a.out ~/bin/fd1d_burgers_lax
#
echo "Normal end of execution."
