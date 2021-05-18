#! /bin/bash
#
gfortran -c -Wall hypersphere_properties_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran hypersphere_properties_test.o $HOME/lib/hypersphere_properties.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hypersphere_properties_test.o
#
mv a.out hypersphere_properties_test
./hypersphere_properties_test > hypersphere_properties_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hypersphere_properties_test
#
echo "Normal end of execution."
