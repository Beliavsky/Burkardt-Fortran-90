#! /bin/bash
#
gfortran -c -Wall doomsday.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv doomsday.o ~/lib/doomsday.o
#
echo "Normal end of execution."
