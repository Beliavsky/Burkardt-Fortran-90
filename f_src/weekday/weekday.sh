#! /bin/bash
#
gfortran -c -Wall weekday.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv weekday.o ~/lib/weekday.o
#
echo "Normal end of execution."
