#! /bin/bash
#
gfortran -c -Wall life_serial.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran life_serial.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm life_serial.o
#
mv a.out $HOME/bin/life_serial
#
echo "Normal end of execution."
