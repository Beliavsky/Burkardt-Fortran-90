#! /bin/bash
#
gfortran -c -Wall subpak_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran subpak_test.o $HOME/lib/subpak.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm subpak_test.o
#
mv a.out subpak_test
./subpak_test > subpak_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm subpak_test
#
echo "Normal end of execution."
