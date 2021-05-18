#! /bin/bash
#
gfortran -c -Wall square_symq_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv square_symq_rule.o ~/lib/square_symq_rule.o
#
echo "Normal end of execution."
