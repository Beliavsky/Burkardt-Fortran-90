#! /bin/bash
#
gfortran -c -Wall stiff_ode_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o stiff_ode_test stiff_ode_test.o $HOME/lib/stiff_ode.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm stiff_ode_test.o
#
./stiff_ode_test > stiff_ode_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm stiff_ode_test
#
gnuplot < stiff_euler_commands.txt
gnuplot < stiff_euler_backward_commands.txt
gnuplot < stiff_midpoint_commands.txt
#
echo "Normal end of execution."
