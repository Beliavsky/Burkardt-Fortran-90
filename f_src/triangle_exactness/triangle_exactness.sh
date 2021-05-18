#! /bin/bash
#
gfortran -c -Wall triangle_exactness.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran triangle_exactness.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm triangle_exactness.o
#
chmod ugo+x a.out
mv a.out ~/bin/triangle_exactness
#
echo "Normal end of execution."
