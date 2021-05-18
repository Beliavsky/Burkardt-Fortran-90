#! /bin/bash
#
gfortran -c -Wall pink_noise_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran pink_noise_test.o $HOME/lib/pink_noise.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm pink_noise_test.o
#
mv a.out pink_noise_test
./pink_noise_test > pink_noise_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm pink_noise_test
#
echo "Normal end of execution."
