#! /bin/bash
#
gfortran -c -Wall cosine_transform_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cosine_transform_test cosine_transform_test.o $HOME/lib/cosine_transform.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cosine_transform_test.o
#
./cosine_transform_test > cosine_transform_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cosine_transform_test
#
echo "Normal end of execution."
