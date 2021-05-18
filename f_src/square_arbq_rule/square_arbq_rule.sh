#! /bin/bash
#
gfortran -c -Wall square_arbq_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv square_arbq_rule.o ~/lib/square_arbq_rule.o
#
echo "Normal end of execution."
