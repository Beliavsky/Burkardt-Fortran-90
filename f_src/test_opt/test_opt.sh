#! /bin/bash
#
gfortran -c -Wall test_opt.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_opt.o ~/lib/test_opt.o
#
echo "Normal end of execution."
