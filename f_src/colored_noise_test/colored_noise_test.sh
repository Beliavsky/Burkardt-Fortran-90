#! /bin/bash
#
gfortran -c -Wall colored_noise_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o colored_noise_test colored_noise_test.o $HOME/lib/colored_noise.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm colored_noise_test.o
#
./colored_noise_test > colored_noise_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm colored_noise_test
#
echo "Normal end of execution."
