#! /bin/bash
#
$HOME/bin/lorenz_ode > lorenz_ode_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
gnuplot < lorenz_ode_commands.txt
#
echo "Normal end of execution."
