#! /bin/bash
#
$HOME/bin/chebyshev1_rule 5 -1 +1 cheby1_o5 > chebyshev1_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
