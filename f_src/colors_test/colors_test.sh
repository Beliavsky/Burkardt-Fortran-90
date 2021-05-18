#! /bin/bash
#
gfortran -c -Wall colors_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o colors_test colors_test.o $HOME/lib/colors.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm colors_test.o
#
./colors_test > colors_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm colors_test
#
echo "Normal end of execution."
