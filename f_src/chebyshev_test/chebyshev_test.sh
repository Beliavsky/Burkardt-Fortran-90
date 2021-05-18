#! /bin/bash
#
gfortran -c -Wall chebyshev_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o chebyshev_test chebyshev_test.o $HOME/lib/chebyshev.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm chebyshev_test.o
#
./chebyshev_test > chebyshev_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm chebyshev_test
#
echo "Normal end of execution."
