#! /bin/bash
#
gfortran -c -Wall memory.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran memory.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm memory.o
#
chmod ugo+x a.out
mv a.out ~/bin/memory
#
echo "Normal end of execution."
