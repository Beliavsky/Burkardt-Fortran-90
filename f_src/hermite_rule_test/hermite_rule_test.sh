#! /bin/bash
#
$HOME/bin/hermite_rule 4 0.0 1.0 0 herm_o4 > hermite_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
