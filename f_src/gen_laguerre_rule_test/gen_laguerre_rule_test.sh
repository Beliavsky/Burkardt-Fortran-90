#! /bin/bash
#
$HOME/bin/gen_laguerre_rule  4 0.5 0.0 1.0 gen_lag_o4_a0.5 > gen_laguerre_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
