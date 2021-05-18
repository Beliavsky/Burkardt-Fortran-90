#! /bin/bash
#
gfortran -c -fno-underscoring -I$HOME/include -Wall fftw_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fftw_test.o -lfftw3 -lm -lc
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm fftw_test.o
#
mv a.out fftw_test
./fftw_test > fftw_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fftw_test
#
echo "Normal end of execution."
