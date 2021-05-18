#! /bin/bash
#
gfortran -c -Wall circle_packing.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran circle_packing.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm circle_packing.o
#
chmod ugo+x a.out
mv a.out ~/bin/circle_packing
#
echo "Normal end of execution."
