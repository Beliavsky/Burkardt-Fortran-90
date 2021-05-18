#! /bin/bash
#
gfortran -c -Wall triangle_to_fem.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran triangle_to_fem.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm triangle_to_fem.o
#
chmod ugo+x a.out
mv a.out ~/bin/triangle_to_fem
#
echo "Normal end of execution."
