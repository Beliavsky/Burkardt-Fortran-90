#! /bin/bash
#
$HOME/bin/gmsh_to_fem example_2d > gmsh_to_fem_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
