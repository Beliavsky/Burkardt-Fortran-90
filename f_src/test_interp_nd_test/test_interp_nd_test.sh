#! /bin/bash
#
gfortran -c -Wall test_interp_nd_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_interp_nd_test.o $HOME/lib/test_interp_nd.o $HOME/lib/r8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_interp_nd_test.o
#
mv a.out test_interp_nd_test
./test_interp_nd_test > test_interp_nd_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_interp_nd_test
#
echo "Normal end of execution."
