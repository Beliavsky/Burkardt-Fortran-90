#! /bin/bash
#
gfortran -c -Wall wathen.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv wathen.o ~/lib/wathen.o
#
echo "Normal end of execution."
