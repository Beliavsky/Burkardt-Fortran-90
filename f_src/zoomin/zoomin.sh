#! /bin/bash
#
gfortran -c -Wall zoomin.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv zoomin.o ~/lib/zoomin.o
#
echo "Normal end of execution."
