#! /bin/bash
#
gfortran -c -Wall album_bar.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran album_bar.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm album_bar.o
#
mv a.out album_bar
./album_bar > album_bar.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm album_bar
#
#  Create graphics.
#
gnuplot < album_commands.txt
#
echo "Normal end of execution."
