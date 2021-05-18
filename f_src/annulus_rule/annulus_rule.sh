#! /bin/bash
#
gfortran -c -Wall annulus_rule.f90
if [ $? -ne 0 ]; then
  echo "Errors compiling " $FILE
  exit
fi
#
mv annulus_rule.o ~/lib
#
echo "Normal end of execution."
