#! /bin/bash
#
gfortran -c -Wall test_opt_con.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_opt_con.o ~/lib/test_opt_con.o
#
echo "Normal end of execution."
