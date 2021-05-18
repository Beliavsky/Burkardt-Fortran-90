#! /bin/bash
#
gfortran -c -Wall sphere_stereograph_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sphere_stereograph_test.o $HOME/lib/sphere_stereograph.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sphere_stereograph_test.o
#
mv a.out sphere_stereograph_test
./sphere_stereograph_test > sphere_stereograph_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sphere_stereograph_test
#
echo "Normal end of execution."
