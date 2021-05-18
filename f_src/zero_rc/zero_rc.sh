#! /bin/bash
#
gfortran -c -Wall zero_rc.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv zero_rc.o ~/lib/zero_rc.o
#
echo "Normal end of execution."
