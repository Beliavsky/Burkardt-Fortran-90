#! /bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 -Wall ex07.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
$DISLIN/bin/gf95link -r8 ex07
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ex07.o
#
./ex07 > ex07.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ex07
#
echo "Normal end of execution."
