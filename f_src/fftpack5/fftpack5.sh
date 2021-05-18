#! /bin/bash
#
gfortran -c -Wall fftpack5.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fftpack5.o ~/lib/fftpack5.o
#
echo "Normal end of execution."
