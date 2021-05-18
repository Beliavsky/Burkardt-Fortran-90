#! /bin/bash
#
gfortran -c -Wall -I/$HOME/include humps.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv humps.o ~/lib/humps.o
#
echo "Normal end of execution."
