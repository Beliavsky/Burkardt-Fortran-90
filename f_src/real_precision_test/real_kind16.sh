#! /bin/bash
#
gfortran -c -Wall real_kind16.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran real_kind16.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm real_kind16.o
#
mv a.out real_kind16
./real_kind16 > real_kind16.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm real_kind16
#
echo "Normal end of execution."
