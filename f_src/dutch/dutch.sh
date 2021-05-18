#! /bin/bash
#
gfortran -c -Wall dutch.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv dutch.o ~/lib/dutch.o
#
echo "Normal end of execution."
