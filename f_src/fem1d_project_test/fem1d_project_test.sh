#! /bin/bash
#
$HOME/bin/fem1d_project sample_powers fem_powers > fem1d_project_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
