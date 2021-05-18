#! /bin/bash
#
gfortran -c -Wall log_normal.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv log_normal.o ~/lib/log_normal.o
#
echo "Normal end of execution."
