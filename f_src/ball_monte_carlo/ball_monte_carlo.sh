#! /bin/bash
#
gfortran -c -Wall ball_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv ball_monte_carlo.o ~/lib/ball_monte_carlo.o
#
echo "Normal end of execution."
