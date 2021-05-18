#! /bin/bash
#
gfortran -c -Wall fd1d_wave_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fd1d_wave_test.o $HOME/lib/fd1d_wave.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fd1d_wave_test.o
#
mv a.out fd1d_wave_test
./fd1d_wave_test > fd1d_wave_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fd1d_wave_test
#
echo "Normal end of execution."
