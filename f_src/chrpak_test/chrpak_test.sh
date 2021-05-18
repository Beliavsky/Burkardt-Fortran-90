#! /bin/bash
#
gfortran -c -Wall chrpak_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o chrpak_test chrpak_test.o $HOME/lib/chrpak.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm chrpak_test.o
#
./chrpak_test > chrpak_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm chrpak_test
#
echo "Normal end of execution."
