#! /bin/bash
#
gfortran -c -Wall haar.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv haar.o ~/lib/haar.o
#
echo "Normal end of execution."
