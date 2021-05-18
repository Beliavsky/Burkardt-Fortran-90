#! /bin/bash
#
gfortran -c -Wall wavelet_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran wavelet_test.o $HOME/lib/wavelet.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm wavelet_test.o
#
mv a.out wavelet_test
./wavelet_test > wavelet_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm wavelet_test
#
echo "Normal end of execution."
