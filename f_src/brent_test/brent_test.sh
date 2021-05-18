#! /bin/bash
#
gfortran -c -Wall brent_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran brent_test.o $HOME/lib/brent.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm brent_test.o
#
mv a.out brent_test
./brent_test > brent_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm brent_test
#
echo "Normal end of execution."
