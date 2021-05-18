#! /bin/bash
#
gfortran -c -Wall lagrange_nd_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran lagrange_nd_test.o $HOME/lib/lagrange_nd.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm lagrange_nd_test.o
#
mv a.out lagrange_nd_test
./lagrange_nd_test > lagrange_nd_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm lagrange_nd_test
#
echo "Normal end of execution."
