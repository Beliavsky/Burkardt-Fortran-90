#! /bin/bash
#
gfortran -c -Wall gegenbauer_exactness.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran gegenbauer_exactness.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm gegenbauer_exactness.o
#
chmod ugo+x a.out
mv a.out ~/bin/gegenbauer_exactness
#
echo "Normal end of execution."
