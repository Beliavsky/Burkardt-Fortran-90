#! /bin/bash
#
gfortran -c -Wall snakes_and_ladders_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o snakes_and_ladders_test snakes_and_ladders_test.o \
  $HOME/lib/snakes_and_ladders.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm snakes_and_ladders_test.o
#
./snakes_and_ladders_test > snakes_and_ladders_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm snakes_and_ladders_test
#
echo "Normal end of execution."
