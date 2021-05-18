#! /bin/bash
#
gfortran -c -Wall stats_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o stats_test stats_test.o /$HOME/lib/stats.o /$HOME/lib/r8lib.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm stats_test.o
#
./stats_test > stats_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm stats_test
#
echo "Normal end of execution."
