#! /bin/bash
#
gfortran -c -Wall rot13.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv rot13.o ~/lib/rot13.o
#
echo "Normal end of execution."
