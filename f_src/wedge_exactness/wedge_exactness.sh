#! /bin/bash
#
gfortran -c -Wall wedge_exactness.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran wedge_exactness.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm wedge_exactness.o
#
chmod ugo+x a.out
mv a.out ~/bin/wedge_exactness
#
echo "Normal end of execution."
