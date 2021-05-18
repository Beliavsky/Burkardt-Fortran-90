#! /bin/bash
#
gfortran -c -Wall md.f90
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
mv a.out md
./md < input.txt > md_O0.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm md
#
echo "Normal end of execution."
