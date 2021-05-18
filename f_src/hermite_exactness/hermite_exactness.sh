#! /bin/bash
#
gfortran -c -Wall hermite_exactness.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran hermite_exactness.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hermite_exactness.o
#
chmod ugo+x a.out
mv a.out ~/bin/hermite_exactness
#
echo "Normal end of execution."
