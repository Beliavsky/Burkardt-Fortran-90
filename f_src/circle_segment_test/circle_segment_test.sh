#! /bin/bash
#
gfortran -c -Wall circle_segment_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o circle_segment_test circle_segment_test.o $HOME/lib/circle_segment.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm circle_segment_test.o
#
./circle_segment_test > circle_segment_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm circle_segment_test
#
echo "Normal end of execution."
