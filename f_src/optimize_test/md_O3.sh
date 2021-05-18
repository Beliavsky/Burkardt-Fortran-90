#! /bin/bash
#
gfortran -c -O3 -Wall md.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran md.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm md.o
#
mv a.out md_O3
./md_O3 < input.txt > md_O3.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm md_O3
#
echo "Normal end of execution."
