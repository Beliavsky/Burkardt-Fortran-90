#! /bin/bash
#
gfortran -c -Wall pink_noise.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv pink_noise.o ~/lib/pink_noise.o
#
echo "Normal end of execution."
