#! /bin/bash
#
gfortran -c -Wall triangulation_q2l.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran triangulation_q2l.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm triangulation_q2l.o
#
chmod ugo+x a.out
mv a.out ~/bin/triangulation_q2l
#
echo "Normal end of execution."
