#! /bin/bash
#
gfortran -c -Wall isbn.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv isbn.o ~/lib/isbn.o
#
echo "Normal end of execution."
