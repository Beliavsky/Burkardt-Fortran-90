#! /bin/bash
#
gfortran -c -Wall simplex_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv simplex_monte_carlo.o ~/lib/simplex_monte_carlo.o
#
echo "Normal end of execution."
