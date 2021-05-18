#! /bin/bash
#
gfortran -c -Wall sftpack_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sftpack_test.o $HOME/lib/sftpack.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sftpack_test.o
#
mv a.out sftpack_test
./sftpack_test > sftpack_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sftpack_test
#
echo "Normal end of execution."
