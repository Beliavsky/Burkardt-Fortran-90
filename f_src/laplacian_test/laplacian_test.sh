#! /bin/bash
#
gfortran -c -Wall laplacian_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran laplacian_test.o $HOME/lib/laplacian.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm laplacian_test.o
#
mv a.out laplacian_test
./laplacian_test > laplacian_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm laplacian_test
#
echo "Normal end of execution."
