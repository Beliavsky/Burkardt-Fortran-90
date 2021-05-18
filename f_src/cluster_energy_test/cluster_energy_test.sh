#! /bin/bash
#
$HOME/bin/cluster_energy < input.txt > cluster_energy_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
