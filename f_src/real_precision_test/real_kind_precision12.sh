#! /bin/bash
#
gfortran -c -Wall real_kind_precision12.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran real_kind_precision12.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm real_kind_precision12.o
#
mv a.out real_kind_precision12
./real_kind_precision12 > real_kind_precision12.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm real_kind_precision12
#
echo "Normal end of execution."
