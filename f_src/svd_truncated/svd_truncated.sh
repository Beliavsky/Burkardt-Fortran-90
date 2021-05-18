#! /bin/bash
#
gfortran -c -Wall svd_truncated.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
#  Use this command normally:
#
gfortran svd_truncated.o -llapack
#
#  Use this command under the Macintosh OSX:
#
#gfortran svd_truncated.o -framework veclib
#
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm svd_truncated.o
#
mv a.out $HOME/bin/svd_truncated
#
echo "Normal end of execution."
