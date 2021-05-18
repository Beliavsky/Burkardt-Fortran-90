#! /bin/bash
#
gfortran -c -Wall barycentric_interp_1d_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o barycentric_interp_1d_test barycentric_interp_1d_test.o $HOME/lib/barycentric_interp_1d.o $HOME/lib/test_interp_1d.o $HOME/lib/r8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm barycentric_interp_1d_test.o
#
./barycentric_interp_1d_test > barycentric_interp_1d_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm barycentric_interp_1d_test
#
echo "Normal end of execution."
