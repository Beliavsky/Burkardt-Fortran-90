#! /bin/bash
#
gfortran -c -Wall cosine_transform.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cosine_transform.o ~/lib/cosine_transform.o
#
echo "Normal end of execution."
