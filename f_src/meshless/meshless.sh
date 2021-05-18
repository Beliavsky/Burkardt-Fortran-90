#! /bin/bash
#
gfortran -c -Wall meshless.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran meshless.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm meshless.o
#
mv a.out ~/bin/meshless
#
echo "Normal end of execution."
