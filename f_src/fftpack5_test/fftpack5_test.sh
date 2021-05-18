#! /bin/bash
#
gfortran -c -Wall fftpack5_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o fftpack5_test fftpack5_test.o $HOME/lib/fftpack5.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fftpack5_test.o
#
./fftpack5_test > fftpack5_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fftpack5_test
#
echo "Normal end of execution."
