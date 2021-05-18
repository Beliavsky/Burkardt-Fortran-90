#! /bin/bash
#
gfortran -c -Wall vector_max_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran vector_max_test.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm vector_max_test.o
#
mv a.out vector_max_test
./vector_max_test > vector_max_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm vector_max_test
#
echo "Normal end of execution."
