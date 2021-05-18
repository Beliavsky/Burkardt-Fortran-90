#! /bin/bash
#
$HOME/bin/laguerre_rule 4 0.0 1.0 lag_o4 > laguerre_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

