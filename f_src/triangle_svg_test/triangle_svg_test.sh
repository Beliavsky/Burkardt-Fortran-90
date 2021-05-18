#! /bin/bash
#
gfortran -c -Wall triangle_svg_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran triangle_svg_test.o $HOME/lib/triangle_svg.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm triangle_svg_test.o
#
mv a.out triangle_svg_test
./triangle_svg_test > triangle_svg_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm triangle_svg_test
#
echo "Normal end of execution."
