#! /bin/bash
#
gfortran -c -O1 -Wall md.f90
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
mv a.out md_O1
./md_O1 < input.txt > md_O1.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm md_O1
#
echo "Normal end of execution."
