#! /bin/bash
#
gfortran -c -Wall sphere_quad_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sphere_quad_test.o $HOME/lib/sphere_quad.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sphere_quad_test.o
#
mv a.out sphere_quad_test
./sphere_quad_test > sphere_quad_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sphere_quad_test
#
echo "Normal end of execution."
