#! /bin/bash
#
gfortran -c -Wall brownian_motion_simulation_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o brownian_motion_simulation_test brownian_motion_simulation_test.o $HOME/lib/brownian_motion_simulation.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm brownian_motion_simulation_test.o
#
./brownian_motion_simulation_test > brownian_motion_simulation_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm brownian_motion_simulation_test
#
echo "Normal end of execution."
