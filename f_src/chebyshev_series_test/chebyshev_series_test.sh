#! /bin/bash
#
gfortran -c -Wall chebyshev_series_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o chebyshev_series_test chebyshev_series_test.o $HOME/lib/chebyshev_series.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm chebyshev_series_test.o
#
./chebyshev_series_test > chebyshev_series_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm chebyshev_series_test
#
echo "Normal end of execution."
