#! /bin/bash
#
$HOME/bin/gen_hermite_rule 4 1.0 0.0 1.0 gen_herm_o4_a1.0 > gen_hermite_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
