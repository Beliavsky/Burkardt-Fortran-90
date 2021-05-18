#! /bin/bash
#
gfortran -c -Wall unicycle_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran unicycle_test.o $HOME/lib/unicycle.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm unicycle_test.o
#
mv a.out unicycle_test
./unicycle_test > unicycle_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm unicycle_test
#
echo "Normal end of execution."
