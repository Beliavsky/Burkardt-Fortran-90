#! /bin/bash
#
gfortran -c -Wall laguerre_exactness.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran laguerre_exactness.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm laguerre_exactness.o
#
chmod ugo+x a.out
mv a.out ~/bin/laguerre_exactness
#
echo "Normal end of execution."
