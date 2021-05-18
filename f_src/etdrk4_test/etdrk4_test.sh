#! /bin/bash
#
gfortran -c -Wall etdrk4_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o etdrk4_test etdrk4_test.o $HOME/lib/etdrk4.o $HOME/lib/fftpack5.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm etdrk4_test.o
#
./etdrk4_test > etdrk4_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm etdrk4_test
#
#  Process the graphics files.
#
gnuplot < burgers_etdrk4_commands.txt
gnuplot < kdv_etdrk4_commands.txt
gnuplot < kdv_ift_commands.txt
#
echo "Normal end of execution."
