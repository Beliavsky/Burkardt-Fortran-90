#! /bin/bash
#
gfortran -c -Wall black_scholes_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o black_scholes_test black_scholes_test.o $HOME/lib/black_scholes.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm black_scholes_test.o
#
./black_scholes_test > black_scholes_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm black_scholes_test
#
echo "Normal end of execution."
