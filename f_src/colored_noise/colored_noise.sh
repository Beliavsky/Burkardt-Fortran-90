#! /bin/bash
#
gfortran -c -Wall colored_noise.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv colored_noise.o ~/lib/colored_noise.o
#
echo "Normal end of execution."
