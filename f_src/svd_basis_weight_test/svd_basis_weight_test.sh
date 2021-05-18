#! /bin/bash
#
$HOME/bin/svd_basis_weight < ex01_input.txt > svd_basis_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
