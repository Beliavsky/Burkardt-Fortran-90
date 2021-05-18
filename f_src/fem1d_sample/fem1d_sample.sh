#! /bin/bash
#
gfortran -c -Wall fem1d_sample.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fem1d_sample.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem1d_sample.o"
  exit
fi
#
rm fem1d_sample.o
#
chmod ugo+x a.out
mv a.out ~/bin/fem1d_sample
#
echo "Normal end of execution."
