#! /bin/bash
#
gfortran -c -Wall stripack_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran stripack_test.o $HOME/lib/stripack.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm stripack_test.o
#
mv a.out stripack_test
./stripack_test > stripack_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm stripack_test
#
ps2png stripack_test_del.eps stripack_test_del.png
rm stripack_test_del.eps
ps2png stripack_test_vor.eps stripack_test_vor.png
rm stripack_test_vor.eps
#
echo "Normal end of execution."
