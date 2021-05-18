#! /bin/bash
#
gfortran -c -Wall table_top.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran table_top.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm table_top.o
#
mv a.out table_top
#
chmod ugo+x table_top
mv table_top $HOME/bin
#
echo "Normal end of execution."
