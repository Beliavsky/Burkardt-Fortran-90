#! /bin/bash
#
$HOME/bincpp/jacobi_rule 4 0.5 1.5 -1.0 +1.0 jac_o4_a0.5_b1.5 > jacobi_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
