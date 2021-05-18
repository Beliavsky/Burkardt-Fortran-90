#! /bin/bash
#
gfortran -c -Wall line_cvt_lloyd.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv line_cvt_lloyd.o ~/lib/line_cvt_lloyd.o
#
echo "Normal end of execution."
