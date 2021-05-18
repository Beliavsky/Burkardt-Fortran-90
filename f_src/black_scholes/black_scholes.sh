#! /bin/bash
#
gfortran -c -Wall black_scholes.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv black_scholes.o ~/lib/black_scholes.o
#
echo "Normal end of execution."
