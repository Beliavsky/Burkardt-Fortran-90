#! /bin/bash
#
gfortran -c -Wall polpak_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran polpak_test.o $HOME/lib/polpak.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm polpak_test.o
#
mv a.out polpak_test
./polpak_test > polpak_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm polpak_test
#
echo "Normal end of execution."
