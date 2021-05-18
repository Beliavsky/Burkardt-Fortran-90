#! /bin/bash
#
gfortran -c -Wall fem2d_bvp_serene_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o fem2d_bvp_serene_test fem2d_bvp_serene_test.o $HOME/lib/fem2d_bvp_serene.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fem2d_bvp_serene_test.o
#
./fem2d_bvp_serene_test > fem2d_bvp_serene_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fem2d_bvp_serene_test
#
echo "Normal end of execution."
