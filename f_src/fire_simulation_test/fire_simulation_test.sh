#! /bin/bash
#
$HOME/bin/fire_simulation > fire_simulation_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
