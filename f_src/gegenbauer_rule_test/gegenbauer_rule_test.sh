#! /bin/bash
#
$HOME/bin/gegenbauer_rule 4 2.0 -1.0 +1.0 gegen_o4_a2.0 > gegenbauer_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
