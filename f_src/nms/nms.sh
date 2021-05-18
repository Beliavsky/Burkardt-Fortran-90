#! /bin/bash
#
gfortran -c -Wall nms.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv nms.o ~/lib/nms.o
#
echo "Normal end of execution."
