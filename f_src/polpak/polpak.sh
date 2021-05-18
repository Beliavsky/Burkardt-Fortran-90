#! /bin/bash
#
gfortran -c -Wall polpak.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv polpak.o ~/lib/polpak.o
#
echo "Normal end of execution."
