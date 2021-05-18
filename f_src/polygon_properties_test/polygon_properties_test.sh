#! /bin/bash
#
gfortran -c -Wall polygon_properties_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran polygon_properties_test.o $HOME/lib/polygon_properties.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm polygon_properties_test.o
#
mv a.out polygon_properties_test
./polygon_properties_test > polygon_properties_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm polygon_properties_test
#
echo "Normal end of execution."
