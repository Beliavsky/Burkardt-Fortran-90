#! /bin/bash
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 -Wall orbital_contour.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
$DISLIN/bin/gf95link -r8 orbital_contour
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm orbital_contour.o
#
./orbital_contour > orbital_contour.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm orbital_contour
#
echo "Normal end of execution."
