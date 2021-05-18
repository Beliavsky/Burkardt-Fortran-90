#! /bin/bash
#
gfortran -c -Wall cvt_box.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran cvt_box.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cvt_box.o
#
chmod ugo+x a.out
mv a.out ~/bin/cvt_box
#
echo "Normal end of execution."
