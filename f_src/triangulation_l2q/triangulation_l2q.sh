#! /bin/bash
#
gfortran -c -Wall triangulation_l2q.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran triangulation_l2q.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm triangulation_l2q.o
#
chmod ugo+x a.out
mv a.out ~/bin/triangulation_l2q
#
echo "Normal end of execution."
