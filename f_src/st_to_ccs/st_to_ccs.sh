#! /bin/bash
#
gfortran -c -Wall st_to_ccs.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv st_to_ccs.o ~/lib/st_to_ccs.o
#
echo "Normal end of execution."
