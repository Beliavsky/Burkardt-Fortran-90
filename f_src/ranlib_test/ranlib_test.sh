#! /bin/bash
#
gfortran -c -Wall ranlib_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran ranlib_test.o $HOME/lib/ranlib.o $HOME/lib/rnglib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ranlib_test.o
#
mv a.out ranlib_test
./ranlib_test > ranlib_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ranlib_test
#
echo "Normal end of execution."
