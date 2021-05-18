#! /bin/bash
#
gfortran -c -Wall floyd_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran floyd_test.o $HOME/lib/floyd.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm floyd_test.o
#
mv a.out floyd_test
./floyd_test > floyd_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm floyd_test
#
echo "Normal end of execution."
