#! /bin/bash
#
gfortran -c -Wall hermite_cubic_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran hermite_cubic_test.o $HOME/lib/hermite_cubic.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hermite_cubic_test.o
#
mv a.out hermite_cubic_test
./hermite_cubic_test > hermite_cubic_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hermite_cubic_test
#
echo "Normal end of execution."
