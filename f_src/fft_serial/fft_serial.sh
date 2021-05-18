#! /bin/bash
#
gfortran -c -Wall fft_serial.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fft_serial.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fft_serial.o
#
mv a.out $HOME/bin/fft_serial
#
echo "Normal end of execution."
