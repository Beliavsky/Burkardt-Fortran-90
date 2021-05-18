#! /bin/bash
#
gfortran -c -Wall stla_io_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran stla_io_test.o $HOME/lib/stla_io.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm stla_io_test.o
#
mv a.out stla_io_test
./stla_io_test > stla_io_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm stla_io_test
#
echo "Normal end of execution."
