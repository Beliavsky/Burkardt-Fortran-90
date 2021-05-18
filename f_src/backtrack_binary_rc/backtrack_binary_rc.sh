#! /bin/bash
#
gfortran -c -Wall backtrack_binary_rc.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv backtrack_binary_rc.o ~/lib/backtrack_binary_rc.o
#
echo "Normal end of execution."
