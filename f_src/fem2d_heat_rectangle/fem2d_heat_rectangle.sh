#! /bin/bash
#
gfortran -c -Wall fem2d_heat_rectangle.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fem2d_heat_rectangle.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem2d_heat_rectangle.o"
  exit
fi
rm fem2d_heat_rectangle.o
#
chmod ugo+x a.out
mv a.out ~/bin/fem2d_heat_rectangle
#
echo "Normal end of execution."
