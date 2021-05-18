#! /bin/bash
#
gfortran -c -Wall fem2d_heat_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
#  Link the precompiled main program FEM2D_HEAT with the user routines.
#
gfortran ~/lib/fem2d_heat.o fem2d_heat_test.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem2d_heat.o + square.o"
  exit
fi
rm fem2d_heat_test.o
#
#  Run the program with the user mesh files.
#
chmod ugo+x a.out
mv a.out fem2d_heat_test
./fem2d_heat_test square_nodes.txt square_elements.txt > fem2d_heat_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fem2d_heat_test
#
#  Convert the EPS files to PNG format.
#
if [ -e elements.eps ]; then
  ps2png elements.eps square_elements.png
  rm elements.eps
fi
#
if [ -e nodes.eps ]; then
  ps2png nodes.eps square_nodes.png
  rm nodes.eps
fi
#
echo "Normal end of execution."
