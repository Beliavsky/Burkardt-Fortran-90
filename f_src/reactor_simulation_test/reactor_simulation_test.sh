#! /bin/bash
#
$HOME/bin/reactor_simulation > reactor_simulation_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
