#! /bin/bash
#
gfortran -c -Wall muller.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv muller.o ~/lib/muller.o
#
echo "Normal end of execution."
