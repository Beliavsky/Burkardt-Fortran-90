#! /bin/bash
#
gfortran -c -Wall calendar_nyt_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o calendar_nyt_test calendar_nyt_test.o $HOME/lib/calendar_nyt.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm calendar_nyt_test.o
#
./calendar_nyt_test > calendar_nyt_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm calendar_nyt_test
#
echo "Normal end of execution."
