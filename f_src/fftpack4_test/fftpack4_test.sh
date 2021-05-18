#! /bin/bash
#
gfortran -c -Wall fftpack4_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o fftpack4_test fftpack4_test.o $HOME/lib/fftpack4.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fftpack4_test.o
#
./fftpack4_test > fftpack4_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fftpack4_test
#
echo "Normal end of execution."
