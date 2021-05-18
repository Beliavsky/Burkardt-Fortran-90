#! /bin/bash
#
gfortran -c -O2 -Wall md.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran md.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm md.o
#
mv a.out ~/bin/md
#
echo "Normal end of execution."
