#! /bin/bash
#
$HOME/bin/fem_to_triangle ell > fem_to_triangle_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
