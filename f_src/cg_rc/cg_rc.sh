#! /bin/bash
#
gfortran -c -Wall cg_rc.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cg_rc.o ~/lib/cg_rc.o
#
echo "Normal end of execution."
