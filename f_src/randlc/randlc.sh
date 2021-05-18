#! /bin/bash
#
gfortran -c -Wall randlc.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv randlc.o ~/lib/randlc.o
#
echo "Normal end of execution."
