#! /bin/bash
#
gfortran -c -Wall circle_segment.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv circle_segment.o ~/lib/circle_segment.o
#
echo "Normal end of execution."
