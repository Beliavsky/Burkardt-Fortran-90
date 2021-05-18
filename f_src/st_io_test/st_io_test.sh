#! /bin/bash
#
gfortran -c -Wall st_io_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran st_io_test.o $HOME/lib/st_io.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm st_io_test.o
#
mv a.out st_io_test
./st_io_test > st_io_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm st_io_test
#
echo "Normal end of execution."
