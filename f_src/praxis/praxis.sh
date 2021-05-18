#! /bin/bash
#
gfortran -c -Wall praxis.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv praxis.o ~/lib/praxis.o
#
echo "Normal end of execution."
