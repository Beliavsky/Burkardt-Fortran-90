#! /bin/bash
#
gfortran -c -Wall fem3d_pack_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fem3d_pack_test.o $HOME/lib/fem3d_pack.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fem3d_pack_test.o
#
mv a.out fem3d_pack_test
./fem3d_pack_test > fem3d_pack_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fem3d_pack_test
#
echo "Normal end of execution."
