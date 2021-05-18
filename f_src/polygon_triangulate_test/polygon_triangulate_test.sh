#! /bin/bash
#
gfortran -c -Wall polygon_triangulate_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran polygon_triangulate_test.o $HOME/lib/polygon_triangulate.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm polygon_triangulate_test.o
#
mv a.out polygon_triangulate_test
./polygon_triangulate_test > polygon_triangulate_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm polygon_triangulate_test
#
gnuplot < comb_commands.txt
gnuplot < hand_commands.txt
gnuplot < i18_commands.txt
#
echo "Normal end of execution."
