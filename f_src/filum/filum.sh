#! /bin/bash
#
gfortran -c -Wall filum.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv filum.o ~/lib/filum.o
#
echo "Normal end of execution."
