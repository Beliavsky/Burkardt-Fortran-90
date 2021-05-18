#! /bin/bash
#
gfortran -c -Wall csv_io_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o csv_io_test csv_io_test.o $HOME/lib/csv_io.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm csv_io_test.o
#
./csv_io_test > csv_io_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm csv_io_test
#
echo "Normal end of execution."
