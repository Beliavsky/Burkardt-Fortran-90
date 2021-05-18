#! /bin/bash
#
$HOME/bin/gen_hermite_exactness gen_herm_o4_a1.0 10 1.0 0 > gen_hermite_exactness_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
