#! /bin/bash
#
$HOME/bin/cellular_automaton > cellular_automaton_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
