#! /bin/bash
#
$HOME/bin/pyramid_rule 2 4 pyramid_l2x2_j4 > pyramid_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

