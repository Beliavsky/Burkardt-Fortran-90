#! /bin/bash
#
$HOME/bin/triangulation_quad example > triangulation_quad_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
