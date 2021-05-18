#! /bin/bash
#
gfortran -c -Wall sine_transform_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sine_transform_test.o $HOME/lib/sine_transform.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sine_transform_test.o
#
mv a.out sine_transform_test
./sine_transform_test > sine_transform_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sine_transform_test
#
echo "Normal end of execution."
