#! /bin/bash
#
gfortran -c -Wall ball_integrals.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv ball_integrals.o ~/lib/ball_integrals.o
#
echo "Normal end of execution."
