#! /bin/bash
#
gfortran -c -Wall pyramid_exactness.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran pyramid_exactness.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm pyramid_exactness.o
#
chmod ugo+x a.out
mv a.out ~/bin/pyramid_exactness
#
echo "Normal end of execution."
