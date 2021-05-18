#! /bin/bash
#
gfortran -c -Wall chebyshev_interp_1d_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o chebyshev_interp_1d_test chebyshev_interp_1d_test.o $HOME/lib/chebyshev_interp_1d.o $HOME/lib/qr_solve.o $HOME/lib/r8lib.o $HOME/lib/test_interp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm chebyshev_interp_1d_test.o
#
./chebyshev_interp_1d_test > chebyshev_interp_1d_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm chebyshev_interp_1d_test
#
echo "Normal end of execution."
