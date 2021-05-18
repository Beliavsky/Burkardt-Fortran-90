#! /bin/bash
#
gfortran -c -Wall nas.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o nas nas.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm nas.o
#
mv nas $HOME/bin/nas
#
echo "Normal end of execution."
