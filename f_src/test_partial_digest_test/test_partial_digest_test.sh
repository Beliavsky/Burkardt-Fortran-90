#! /bin/bash
#
gfortran -c -Wall test_partial_digest_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o test_partial_digest_test test_partial_digest_test.o \
  $HOME/lib/test_partial_digest.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_partial_digest_test.o
#
./test_partial_digest_test > test_partial_digest_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_partial_digest_test
#
echo "Normal end of execution."
