#! /bin/bash
#
gfortran -c -Wall pbma_io_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran pbma_io_test.o $HOME/lib/pbma_io.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm pbma_io_test.o
#
mv a.out pbma_io_test
./pbma_io_test > pbma_io_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm pbma_io_test
#
echo "Normal end of execution."
