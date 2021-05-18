#! /bin/bash
#
gfortran -c -Wall lapack_d_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran lapack_d_test.o $HOME/lib/lapack_d.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm lapack_d_test.o
#
mv a.out lapack_d_test
./lapack_d_test > lapack_d_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm lapack_d_test
#
echo "Normal end of execution."
