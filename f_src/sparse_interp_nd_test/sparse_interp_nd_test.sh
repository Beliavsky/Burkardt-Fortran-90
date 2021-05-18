#! /bin/bash
#
gfortran -c -Wall sparse_interp_nd_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sparse_interp_nd_test.o $HOME/lib/sparse_interp_nd.o $HOME/lib/r8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sparse_interp_nd_test.o
#
mv a.out sparse_interp_nd_test
./sparse_interp_nd_test > sparse_interp_nd_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sparse_interp_nd_test
#
echo "Normal end of execution."
