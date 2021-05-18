#! /bin/bash
#
gfortran -c -Wall wavelet.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv wavelet.o ~/lib/wavelet.o
#
echo "Normal end of execution."
