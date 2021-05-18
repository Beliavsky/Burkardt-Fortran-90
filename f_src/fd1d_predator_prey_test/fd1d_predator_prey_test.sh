#! /bin/bash
#
gfortran -c -Wall fd1d_predator_prey_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fd1d_predator_prey_test.o $HOME/lib/fd1d_predator_prey.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fd1d_predator_prey_test.o
#
mv a.out fd1d_predator_prey_test
./fd1d_predator_prey_test < input.txt > fd1d_predator_prey_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fd1d_predator_prey_test
#
gnuplot < uv_commands.txt
#
echo "Normal end of execution."
