#! /bin/bash
#
h5fc hdf5_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
rm hdf5_test.o
mv a.out hdf5_test
#
./hdf5_test > hdf5_test.txt
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hdf5_test
#
echo "Normal end of execution."
