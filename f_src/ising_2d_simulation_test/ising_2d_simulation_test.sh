#! /bin/bash
#
$HOME/bin/ising_2d_simulation 10 10 15 0.5 123456789 > ising_2d_simulation_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

