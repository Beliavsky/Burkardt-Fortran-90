#! /bin/bash
#
gfortran -c -Wall heated_plate.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran heated_plate.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm heated_plate.o
#
chmod ugo+x a.out
mv a.out ~/bin/heated_plate
#
echo "Normal end of execution."
