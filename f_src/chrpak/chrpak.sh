#! /bin/bash
#
gfortran -c -Wall chrpak.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv chrpak.o ~/lib/chrpak.o
#
echo "Normal end of execution."
