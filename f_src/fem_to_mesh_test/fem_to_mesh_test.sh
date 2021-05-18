#! /bin/bash
#
$HOME/bin/fem_to_mesh ell > fem_to_mesh_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
$HOME/bin/fem_to_mesh p01 >> fem_to_mesh_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
