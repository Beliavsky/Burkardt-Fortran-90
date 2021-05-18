#! /bin/bash
#
$HOME/bin/ising_3d_simulation 10 25 > ising_3d_simulation_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

