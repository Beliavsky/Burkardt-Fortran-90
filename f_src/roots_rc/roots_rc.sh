#! /bin/bash
#
gfortran -c -Wall roots_rc.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv roots_rc.o ~/lib/roots_rc.o
#
echo "Normal end of execution."
