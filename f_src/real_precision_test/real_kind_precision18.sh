#! /bin/bash
#
gfortran -c -Wall real_kind_precision18.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran real_kind_precision18.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm real_kind_precision18.o
#
mv a.out real_kind_precision18
./real_kind_precision18 > real_kind_precision18.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm real_kind_precision18
#
echo "Normal end of execution."
