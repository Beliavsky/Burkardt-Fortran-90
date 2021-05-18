#! /bin/bash
#
gfortran -c -Wall weekday_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran weekday_test.o $HOME/lib/weekday.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm weekday_test.o
#
mv a.out weekday_test
./weekday_test > weekday_test.txt
if [ $? -ne 0 ]; then
  echo "Runtime error."
  exit
fi
rm weekday_test
#
echo "Normal end of execution."
