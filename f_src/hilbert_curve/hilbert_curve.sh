#! /bin/bash
#
gfortran -c -Wall hilbert_curve.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hilbert_curve.o ~/lib/hilbert_curve.o
#
echo "Normal end of execution."
