#! /bin/bash
#
gfortran -c -Wall fastgl_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o fastgl_test fastgl_test.o $HOME/lib/fastgl.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fastgl_test.o
#
./fastgl_test > fastgl_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fastgl_test
#
echo "Normal end of execution."
