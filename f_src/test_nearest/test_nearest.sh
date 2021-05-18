#! /bin/bash
#
gfortran -c -Wall test_nearest.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_nearest.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_nearest.o
#
chmod ugo+x a.out
mv a.out ~/bin/test_nearest
#
echo "Normal end of execution."
