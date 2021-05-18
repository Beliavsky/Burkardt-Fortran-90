#! /bin/bash
#
gfortran -c -Wall satisfy.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran satisfy.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm satisfy.o
#
mv a.out $HOME/bin/satisfy
#
echo "Normal end of execution."
