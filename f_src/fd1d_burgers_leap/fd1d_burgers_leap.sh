#! /bin/bash
#
gfortran -c -Wall fd1d_burgers_leap.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fd1d_burgers_leap.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fd1d_burgers_leap.o
#
mv a.out ~/bin/fd1d_burgers_leap
#
echo "Normal end of execution."
