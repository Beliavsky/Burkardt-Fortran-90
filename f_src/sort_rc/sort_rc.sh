#! /bin/bash
#
gfortran -c -Wall sort_rc.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sort_rc.o ~/lib/sort_rc.o
#
echo "Normal end of execution."
