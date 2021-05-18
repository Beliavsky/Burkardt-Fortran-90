#! /bin/bash
#
gfortran -c -Wall file_transpose.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran file_transpose.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm file_transpose.o
#
chmod ugo+x a.out
mv a.out ~/bin/file_transpose
#
echo "Normal end of execution."
