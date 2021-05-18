#! /bin/bash
#
gfortran -c -Wall ppma_io_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran ppma_io_test.o $HOME/lib/ppma_io.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ppma_io_test.o
#
mv a.out ppma_io_test
./ppma_io_test > ppma_io_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ppma_io_test
#
convert test01.ascii.ppm test01.png
convert test02.ascii.ppm test02.png
#
echo "Normal end of execution."
