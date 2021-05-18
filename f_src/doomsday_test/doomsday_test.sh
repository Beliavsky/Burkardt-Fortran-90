#! /bin/bash
#
gfortran -c -Wall doomsday_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o doomsday_test doomsday_test.o /$HOME/lib/doomsday.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm doomsday_test.o
#
./doomsday_test > doomsday_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm doomsday_test
#
echo "Normal end of execution."
