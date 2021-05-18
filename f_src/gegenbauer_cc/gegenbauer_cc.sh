#! /bin/bash
#
gfortran -c -Wall gegenbauer_cc.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv gegenbauer_cc.o ~/lib/gegenbauer_cc.o
#
echo "Normal end of execution."
