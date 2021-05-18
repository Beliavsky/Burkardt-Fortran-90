#! /bin/bash
#
gfortran -c -Wall van_der_corput_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran van_der_corput_test.o $HOME/lib/van_der_corput.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm van_der_corput_test.o
#
mv a.out van_der_corput_test
./van_der_corput_test > van_der_corput_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm van_der_corput_test
#
echo "Normal end of execution."
