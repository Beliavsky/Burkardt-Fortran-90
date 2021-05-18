#! /bin/bash
#
gfortran -c -Wall fftpack4.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fftpack4.o ~/lib/fftpack4.o
#
echo "Normal end of execution."
