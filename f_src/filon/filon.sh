#! /bin/bash
#
gfortran -c -Wall filon.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv filon.o ~/lib/filon.o
#
echo "Normal end of execution."
