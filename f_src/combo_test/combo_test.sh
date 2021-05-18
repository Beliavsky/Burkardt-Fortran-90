#! /bin/bash
#
gfortran -c -Wall combo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o combo_test combo_test.o $HOME/lib/combo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm combo_test.o
#
./combo_test > combo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm combo_test
#
echo "Normal end of execution."
