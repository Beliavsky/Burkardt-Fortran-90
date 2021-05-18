#! /bin/bash
#
gfortran -c -Wall square_minimal_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv square_minimal_rule.o ~/lib/square_minimal_rule.o
#
echo "Normal end of execution."
