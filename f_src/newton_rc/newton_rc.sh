#! /bin/bash
#
gfortran -c -Wall newton_rc.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv newton_rc.o ~/lib/newton_rc.o
#
echo "Normal end of execution."
