#! /bin/bash
#
gfortran -c -Wall qr_solve.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv qr_solve.o ~/lib/qr_solve.o
#
echo "Normal end of execution."
