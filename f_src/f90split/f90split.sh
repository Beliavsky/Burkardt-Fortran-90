#! /bin/bash
#
gfortran -c -Wall f90split.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran f90split.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm f90split.o
#
chmod ugo+x a.out
mv a.out ~/bin/f90split
#
echo "Normal end of execution."
