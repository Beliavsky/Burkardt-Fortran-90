#! /bin/bash
#
$HOME/bin/mesh_bandwidth hex_holes6.txt > mesh_bandwidth_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
