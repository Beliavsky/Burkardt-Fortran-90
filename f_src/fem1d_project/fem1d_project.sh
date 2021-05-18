#! /bin/bash
#
gfortran -c -Wall fem1d_project.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fem1d_project.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm fem1d_project.o
#
chmod ugo+x a.out
mv a.out ~/bin/fem1d_project
#
echo "Normal end of execution."
