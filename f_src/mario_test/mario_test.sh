#! /bin/bash
#
$HOME/bin/mario > mario_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
#  Now run gnuplot.
#
gnuplot < mario_commands.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
