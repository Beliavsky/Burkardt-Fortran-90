#! /bin/bash
#
gfortran -c -Wall subanagram.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran subanagram.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm subanagram.o
#
mv a.out ~/bin/subanagram
#
echo "Normal end of execution."
