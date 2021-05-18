#! /bin/bash
#
gfortran -c -Wall cvtp_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cvtp_test cvtp_test.o $HOME/lib/cvtp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cvtp_test.o
#
./cvtp_test > cvtp_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cvtp_test
#
echo "Normal end of execution."
