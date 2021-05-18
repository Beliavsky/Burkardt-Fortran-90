#! /bin/bash
#
gfortran -c -Wall subpak.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv subpak.o ~/lib/subpak.o
#
echo "Normal end of execution."
