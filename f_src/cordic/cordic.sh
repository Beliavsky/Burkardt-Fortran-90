#! /bin/bash
#
gfortran -c -Wall cordic.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cordic.o ~/lib/cordic.o
#
echo "Normal end of execution."
