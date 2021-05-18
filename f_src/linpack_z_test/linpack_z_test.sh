#! /bin/bash
#
gfortran -c -Wall linpack_z_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran linpack_z_test.o $HOME/lib/linpack_z.o $HOME/lib/blas1_z.o \
  $HOME/lib/blas0.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm linpack_z_test.o
#
mv a.out linpack_z_test
./linpack_z_test > linpack_z_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm linpack_z_test
#
echo "Normal end of execution."
