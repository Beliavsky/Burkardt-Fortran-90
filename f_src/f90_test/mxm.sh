#! /bin/bash
#
gfortran -c -Wall mxm.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran mxm.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm mxm.o
#
mv a.out mxm
./mxm > mxm_output.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm mxm
#
echo "Normal end of execution."
