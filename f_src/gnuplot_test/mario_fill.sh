#! /bin/bash
#
gfortran -c -Wall mario_fill.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o mario_fill mario_fill.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm mario_fill.o
#
./mario_fill > mario_fill.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm mario_fill
#
#  Now run gnuplot.
#
gnuplot < mario_fill_commands.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
