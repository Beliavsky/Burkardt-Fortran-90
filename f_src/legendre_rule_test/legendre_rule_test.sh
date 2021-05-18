#! /bin/bash
#
$HOME/bin/legendre_rule 4 -1.0 1.0 leg_o4 > legendre_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

