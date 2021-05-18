#! /bin/bash
#
gfortran -c -Wall change_making.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv change_making.o ~/lib/change_making.o
#
echo "Normal end of execution."
