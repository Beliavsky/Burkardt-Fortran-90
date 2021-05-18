#! /bin/bash
#
gfortran -c -Wall fftpack51_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o fftpack51_test fftpack51_test.o $HOME/lib/fftpack51.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fftpack51_test.o
#
./fftpack51_test > fftpack51_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fftpack51_test
#
echo "Normal end of execution."
