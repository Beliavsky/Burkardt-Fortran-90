#! /bin/bash
#
gfortran -c -Wall simplex_gm_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv simplex_gm_rule.o ~/lib/simplex_gm_rule.o
#
echo "Normal end of execution."
