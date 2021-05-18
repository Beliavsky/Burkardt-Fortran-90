#! /bin/bash
#
gfortran -c -Wall shallow_water_1d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran shallow_water_1d.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm shallow_water_1d.o
#
chmod ugo+x a.out
mv a.out ~/bin/shallow_water_1d
#
echo "Normal end of execution."
