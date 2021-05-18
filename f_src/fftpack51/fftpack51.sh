#! /bin/bash
#
gfortran -c -Wall fftpack51.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fftpack51.o ~/lib/fftpack51.o
#
echo "Normal end of execution."
