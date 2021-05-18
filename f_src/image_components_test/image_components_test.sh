#! /bin/bash
#
gfortran -c -Wall image_components_test.f90
if [ $? -ne 0 ]; then
    echo "Compile error."
  exit
fi
#
gfortran image_components_test.o $HOME/lib/image_components.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm image_components_test.o
#
mv a.out image_components_test
./image_components_test > image_components_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm image_components_test
#
echo "Normal end of execution."
