#! /bin/bash
#
gfortran -c -Wall machar.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv machar.o ~/lib/machar.o
#
echo "Normal end of execution."
