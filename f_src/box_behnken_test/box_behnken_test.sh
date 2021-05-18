#! /bin/bash
#
gfortran -c -Wall box_behnken_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o box_behnken_test box_behnken_test.o $HOME/lib/box_behnken.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm box_behnken_test.o
#
./box_behnken_test > box_behnken_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm box_behnken_test
#
echo "Normal end of execution."
