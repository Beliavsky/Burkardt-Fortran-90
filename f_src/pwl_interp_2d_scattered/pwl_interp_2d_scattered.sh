#! /bin/bash
#
gfortran -c -Wall pwl_interp_2d_scattered.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv pwl_interp_2d_scattered.o ~/lib/pwl_interp_2d_scattered.o
#
echo "Normal end of execution."
