#! /bin/bash
#
gfortran -c -Wall lau_np.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv lau_np.o ~/lib/lau_np.o
#
echo "Normal end of execution."
