#! /bin/bash
#
gfortran -c -Wall llsq_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran llsq_test.o $HOME/lib/llsq.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm llsq_test.o
#
mv a.out llsq_test
./llsq_test > llsq_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm llsq_test
#
echo "Normal end of execution."
