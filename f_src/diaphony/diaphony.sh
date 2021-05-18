#! /bin/bash
#
gfortran -c -Wall diaphony.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran diaphony.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm diaphony.o
#
chmod ugo+x a.out
mv a.out ~/bin/diaphony
#
echo "Normal end of execution."
