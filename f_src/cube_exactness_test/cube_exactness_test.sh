#! /bin/bash
#
gfortran -c -Wall cube_exactness_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cube_exactness_test cube_exactness_test.o $HOME/lib/cube_exactness.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cube_exactness_test.o
#
./cube_exactness_test > cube_exactness_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cube_exactness_test
#
echo "Normal end of execution."
