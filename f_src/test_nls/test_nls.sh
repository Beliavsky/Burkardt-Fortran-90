#! /bin/bash
#
gfortran -c -Wall test_nls.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_nls.o ~/lib/test_nls.o
#
echo "Normal end of execution."
