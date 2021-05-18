#! /bin/bash
#
gfortran -c -Wall sparse_display_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o sparse_display_test sparse_display_test.o $HOME/lib/sparse_display.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sparse_display_test.o
#
./sparse_display_test > sparse_display_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sparse_display_test
#
echo "Normal end of execution."
