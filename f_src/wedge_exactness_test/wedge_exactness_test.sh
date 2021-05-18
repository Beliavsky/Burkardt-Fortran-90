#! /bin/bash
#
$HOME/bin/wedge_exactness wedge_felippa_3x2 5 > wedge_exactness_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

