#! /bin/bash
#
gfortran -c -Wall fd1d_wave.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fd1d_wave.o ~/lib/fd1d_wave.o
#
echo "Normal end of execution."
