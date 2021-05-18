#! /bin/bash
#
gfortran -c -Wall ccs_io_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o ccs_io_test ccs_io_test.o $HOME/lib/ccs_io.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ccs_io_test.o
#
./ccs_io_test > ccs_io_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ccs_io_test
#
echo "Normal end of execution."
