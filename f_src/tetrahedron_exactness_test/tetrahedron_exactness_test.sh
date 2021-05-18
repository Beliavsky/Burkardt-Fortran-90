#! /bin/bash
#
$HOME/bin/tetrahedron_exactness keast3 5 > tetrahedron_exactness_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

