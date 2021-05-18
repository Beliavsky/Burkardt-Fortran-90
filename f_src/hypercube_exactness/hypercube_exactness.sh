#! /bin/bash
#
gfortran -c -Wall hypercube_exactness.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran hypercube_exactness.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hypercube_exactness.o
#
chmod ugo+x a.out
mv a.out ~/bin/hypercube_exactness
#
echo "Normal end of execution."
