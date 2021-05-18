#! /bin/bash
#
gfortran -c -Wall edge_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran edge_test.o $HOME/lib/edge.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm edge_test.o
#
mv a.out edge_test
./edge_test > edge_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm edge_test
#
gnuplot < fx1_commands.txt
gnuplot < fx2_commands.txt
gnuplot < fx3_commands.txt
gnuplot < fx4_commands.txt
gnuplot < fx5_commands.txt
gnuplot < fx6_commands.txt
gnuplot < fx7_commands.txt
gnuplot < fxy1_commands.txt
gnuplot < fxy2_commands.txt
gnuplot < fxy3_commands.txt
gnuplot < fxy4_commands.txt
gnuplot < fxy5_commands.txt
gnuplot < fxyz1_x_commands.txt
gnuplot < fxyz1_y_commands.txt
gnuplot < fxyz1_z_commands.txt
#
echo "Normal end of execution."
