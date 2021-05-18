#! /bin/bash
#
$HOME/bin/ccn_rule 9 -1 +1 ccn_o9 > ccn_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
