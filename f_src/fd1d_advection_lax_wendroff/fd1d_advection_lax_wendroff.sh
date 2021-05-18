#! /bin/bash
#
gfortran -c -Wall fd1d_advection_lax_wendroff.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fd1d_advection_lax_wendroff.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm fd1d_advection_lax_wendroff.o
mv a.out ~/bin/fd1d_advection_lax_wendroff
#
echo "Normal end of execution."
