#! /bin/bash
#
gfortran -c -Wall quadmom_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran quadmom_test.o $HOME/lib/quadmom.o $HOME/lib/toms655.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm quadmom_test.o
#
mv a.out quadmom_test
./quadmom_test > quadmom_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm quadmom_test
#
echo "Normal end of execution."
