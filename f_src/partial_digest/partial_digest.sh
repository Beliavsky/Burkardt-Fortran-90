#! /bin/bash
#
gfortran -c -Wall partial_digest.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv partial_digest.o ~/lib/partial_digest.o
#
echo "Normal end of execution."
