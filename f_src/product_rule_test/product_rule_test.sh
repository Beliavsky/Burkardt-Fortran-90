#! /bin/bash
#
$HOME/bin/product_rule factors.txt ccgl_d2_o3x2 > product_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
