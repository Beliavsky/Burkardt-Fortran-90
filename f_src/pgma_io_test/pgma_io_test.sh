#! /bin/bash
#
gfortran -c -Wall pgma_io_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran pgma_io_test.o $HOME/lib/pgma_io.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm pgma_io_test.o
#
mv a.out pgma_io_test
./pgma_io_test > pgma_io_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm pgma_io_test
#
echo "Normal end of execution."
