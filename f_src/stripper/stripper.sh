#! /bin/bash
#
gfortran -c -Wall stripper.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran stripper.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm stripper.o
#
chmod ugo+x a.out
mv a.out ~/bin/stripper
#
echo "Normal end of execution."
