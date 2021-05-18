#! /bin/bash
#
gfortran -c -Wall mario.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o mario mario.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm mario.o
#
mv mario $HOME/bin/mario
#
echo "Normal end of execution."
