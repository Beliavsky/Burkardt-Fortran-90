#! /bin/bash
#
gfortran -c -Wall wtime_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran wtime_test.o $HOME/lib/wtime.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm wtime_test.o
#
mv a.out wtime_test
./wtime_test > wtime_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm wtime_test
#
echo "Normal end of execution."
