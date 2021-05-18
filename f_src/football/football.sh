#! /bin/bash
#
gfortran -c -Wall football.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran football.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm football.o
#
mv a.out ~/bin/football
#
echo "Normal end of execution."
