#! /bin/bash
#
gfortran -c -Wall cycle_brent_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cycle_brent_test cycle_brent_test.o $HOME/lib/cycle_brent.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cycle_brent_test.o
#
./cycle_brent_test > cycle_brent_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cycle_brent_test
#
echo "Normal end of execution."
