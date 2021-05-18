#! /bin/bash
#
gfortran -c -Wall maxmin_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran maxmin_test.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm maxmin_test.o
#
mv a.out maxmin_test
./maxmin_test > maxmin_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm maxmin_test
#
echo "Normal end of execution."
