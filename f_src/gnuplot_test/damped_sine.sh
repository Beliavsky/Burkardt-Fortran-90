#! /bin/bash
#
gfortran -c -Wall damped_sine.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran damped_sine.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm damped_sine.o
mv a.out ~/bin/damped_sine
#
echo "Normal end of execution."
