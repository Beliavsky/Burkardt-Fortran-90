#! /bin/bash
#
gfortran -c -Wall triangle_io_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o triangle_io_test triangle_io_test.o $HOME/lib/triangle_io.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm triangle_io_test.o
#
./triangle_io_test > triangle_io_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm triangle_io_test
#
echo "Normal end of execution."
