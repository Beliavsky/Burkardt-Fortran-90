#! /bin/bash
#
gfortran -c -Wall timestamp_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran timestamp_test.o $HOME/lib/timestamp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm timestamp_test.o
#
mv a.out timestamp_test
./timestamp_test > timestamp_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm timestamp_test
#
echo "Normal end of execution."
