#! /bin/bash
#
gfortran -c -Wall grf_io_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran grf_io_test.o $HOME/lib/grf_io.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm grf_io_test.o
#
mv a.out grf_io_test
./grf_io_test > grf_io_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm grf_io_test
#
echo "Normal end of execution."
