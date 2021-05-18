#! /bin/bash
#
gfortran -c -Wall image_denoise.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv image_denoise.o ~/lib/image_denoise.o
#
echo "Normal end of execution."
