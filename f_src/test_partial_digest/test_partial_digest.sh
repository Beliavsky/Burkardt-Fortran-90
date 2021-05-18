#! /bin/bash
#
gfortran -c -Wall test_partial_digest.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_partial_digest.o ~/lib/test_partial_digest.o
#
echo "Normal end of execution."
