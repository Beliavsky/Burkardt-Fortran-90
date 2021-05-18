#! /bin/bash
#
gfortran -c -Wall index.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv index.o ~/lib/index.o
#
echo "Normal end of execution."
