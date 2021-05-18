#! /bin/bash
#
gfortran -c -Wall bisection_rc.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv bisection_rc.o ~/lib/bisection_rc.o
#
echo "Normal end of execution."
