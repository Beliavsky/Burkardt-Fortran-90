#! /bin/bash
#
gfortran -c -Wall -I/$HOME/include hankel_cholesky.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hankel_cholesky.o ~/lib/hankel_cholesky.o
#
echo "Normal end of execution."
