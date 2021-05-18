#! /bin/bash
#
gfortran -c -Wall padua_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran padua_test.o $HOME/lib/padua.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm padua_test.o
#
mv a.out padua_test
./padua_test > padua_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm padua_test
#
gnuplot < padua_01_commands.txt
gnuplot < padua_02_commands.txt
gnuplot < padua_03_commands.txt
gnuplot < padua_04_commands.txt
gnuplot < padua_05_commands.txt
gnuplot < padua_06_commands.txt
gnuplot < padua_07_commands.txt
gnuplot < padua_08_commands.txt
gnuplot < padua_09_commands.txt
gnuplot < padua_10_commands.txt
#
echo "Normal end of execution."
