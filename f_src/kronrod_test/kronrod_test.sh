#! /bin/bash
#
gfortran -c -Wall kronrod_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran kronrod_test.o $HOME/lib/kronrod.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm kronrod_test.o
#
mv a.out kronrod_test
./kronrod_test > kronrod_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm kronrod_test
#
echo "Normal end of execution."
