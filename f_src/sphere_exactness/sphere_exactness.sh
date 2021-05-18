#! /bin/bash
#
gfortran -c -Wall sphere_exactness.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sphere_exactness.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sphere_exactness.o
#
chmod ugo+x a.out
mv a.out ~/bin/sphere_exactness
#
echo "Normal end of execution."
