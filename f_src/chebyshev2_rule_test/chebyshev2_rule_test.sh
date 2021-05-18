#! /bin/bash
#
$HOME/bin/chebyshev2_rule 5 -1 +1 cheby2_o5 > chebyshev2_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
