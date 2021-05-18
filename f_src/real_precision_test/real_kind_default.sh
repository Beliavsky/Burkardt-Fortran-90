#! /bin/bash
#
gfortran -c -Wall real_kind_default.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran real_kind_default.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm real_kind_default.o
#
mv a.out real_kind_default
./real_kind_default > real_kind_default.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm real_kind_default
#
echo "Normal end of execution."
