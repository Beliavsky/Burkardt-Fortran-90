#! /bin/bash
#
gfortran -c -Wall triangle_properties_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o triangle_properties_test triangle_properties_test.o \
  $HOME/lib/triangle_properties.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm triangle_properties_test.o
#
./triangle_properties_test > triangle_properties_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm triangle_properties_test
#
echo "Normal end of execution."
