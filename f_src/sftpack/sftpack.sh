#! /bin/bash
#
gfortran -c -Wall sftpack.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sftpack.o ~/lib/sftpack.o
#
echo "Normal end of execution."
