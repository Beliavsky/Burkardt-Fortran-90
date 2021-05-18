#! /bin/bash
#
$HOME/bin/legendre_rule_fast 15 0.0 2.0 > legendre_rule_fast_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

