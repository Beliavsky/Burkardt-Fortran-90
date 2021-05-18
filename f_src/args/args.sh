#! /bin/bash
#
gfortran -c -Wall args.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran args.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm args.o
#
mv a.out ~/bin/args
#
echo "Normal end of execution."
