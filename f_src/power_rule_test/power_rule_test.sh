#! /bin/bash
#
$HOME/bin/power_rule cc3 2 > power_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
