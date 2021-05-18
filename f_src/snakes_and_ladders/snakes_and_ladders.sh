#! /bin/bash
#
gfortran -c -Wall snakes_and_ladders.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv snakes_and_ladders.o ~/lib/snakes_and_ladders.o
#
echo "Normal end of execution."
