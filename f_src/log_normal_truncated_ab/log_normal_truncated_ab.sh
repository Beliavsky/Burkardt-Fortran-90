#! /bin/bash
#
gfortran -c -Wall log_normal_truncated_ab.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv log_normal_truncated_ab.o ~/lib/log_normal_truncated_ab.o
#
echo "Normal end of execution."
