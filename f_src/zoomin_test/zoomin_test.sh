#! /bin/bash
#
gfortran -c -Wall zoomin_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o zoomin_test zoomin_test.o $HOME/lib/zoomin.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm zoomin_test.o
#
./zoomin_test > zoomin_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm zoomin_test
#
echo "Normal end of execution."
