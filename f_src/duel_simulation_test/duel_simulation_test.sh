#! /bin/bash
#
$HOME/bin/duel_simulation < input.txt > duel_simulation_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
