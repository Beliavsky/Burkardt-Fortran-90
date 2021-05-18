#! /bin/bash
#
$HOME/bin/fair_dice_simulation 500 > fair_dice_simulation_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
gnuplot < fair_dice_commands.txt
#
echo "Normal end of execution."
