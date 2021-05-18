#! /bin/bash
#
gfortran -c -Wall legendre_exactness.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran legendre_exactness.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm legendre_exactness.o
#
chmod ugo+x a.out
mv a.out ~/bin/legendre_exactness
#
echo "Normal end of execution."
