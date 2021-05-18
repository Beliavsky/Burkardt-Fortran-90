#! /bin/bash
#
gfortran -c -Wall laupack_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran laupack_test.o $HOME/lib/laupack.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm laupack_test.o
#
mv a.out laupack_test
./laupack_test > laupack_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm laupack_test
#
echo "Normal end of execution."
