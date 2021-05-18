#! /bin/bash
#
gfortran -c -Wall ornstein_uhlenbeck.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv ornstein_uhlenbeck.o ~/lib/ornstein_uhlenbeck.o
#
echo "Normal end of execution."
