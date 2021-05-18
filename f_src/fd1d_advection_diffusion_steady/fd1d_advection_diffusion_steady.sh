#! /bin/bash
#
gfortran -c -Wall fd1d_advection_diffusion_steady.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fd1d_advection_diffusion_steady.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm fd1d_advection_diffusion_steady.o
mv a.out ~/bin/fd1d_advection_diffusion_steady
#
echo "Normal end of execution."
