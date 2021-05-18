#! /bin/bash
#
gfortran -c -Wall real_kind_precision24.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran real_kind_precision24.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm real_kind_precision24.o
#
mv a.out real_kind_precision24
./real_kind_precision24 > real_kind_precision24.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm real_kind_precision24
#
echo "Normal end of execution."
