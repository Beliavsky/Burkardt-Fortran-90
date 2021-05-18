#! /bin/bash
#
gfortran -c -Wall mgmres_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran mgmres_test.o $HOME/lib/mgmres.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm mgmres_test.o
#
mv a.out mgmres_test
./mgmres_test > mgmres_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm mgmres_test
#
echo "Normal end of execution."
