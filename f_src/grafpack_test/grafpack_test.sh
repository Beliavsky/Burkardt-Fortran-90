#! /bin/bash
#
gfortran -c -Wall grafpack_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran grafpack_test.o $HOME/lib/grafpack.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm grafpack_test.o
#
mv a.out grafpack_test
./grafpack_test > grafpack_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm grafpack_test
#
echo "Normal end of execution."
