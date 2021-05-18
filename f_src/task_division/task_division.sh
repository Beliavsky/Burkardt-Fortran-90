#! /bin/bash
#
gfortran -c -Wall task_division.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv task_division.o ~/lib/task_division.o
#
echo "Normal end of execution."
