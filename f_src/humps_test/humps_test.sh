#! /bin/bash
#
gfortran -c -Wall humps_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o humps_test humps_test.o /$HOME/lib/humps.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm humps_test.o
#
./humps_test > humps_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm humps_test
#
echo "Normal end of execution."
