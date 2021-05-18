#! /bin/bash
#
gfortran -c -Wall exponential.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran exponential.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm exponential.o
#
mv a.out exponential
./exponential > exponential.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm exponential
#
echo "Normal end of execution."
