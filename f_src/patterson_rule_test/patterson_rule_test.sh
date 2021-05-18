#! /bin/bash
#
$HOME/bin/patterson_rule 15 -1.0 +1.0 gp_o15 > patterson_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
