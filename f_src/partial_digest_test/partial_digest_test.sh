#! /bin/bash
#
gfortran -c -Wall partial_digest_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o partial_digest_test partial_digest_test.o $HOME/lib/partial_digest.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm partial_digest_test.o
#
./partial_digest_test > partial_digest_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm partial_digest_test
#
echo "Normal end of execution."
