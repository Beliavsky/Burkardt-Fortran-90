#! /bin/bash
#
gfortran -c -Wall ranmap.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran ranmap.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm ranmap.o
mv a.out ~/bin/ranmap
#
echo "Normal end of execution."
