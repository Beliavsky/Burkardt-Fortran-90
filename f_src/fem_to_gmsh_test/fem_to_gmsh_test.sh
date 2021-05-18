#! /bin/bash
#
$HOME/bin/fem_to_gmsh cheby9 > fem_to_gmsh_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
$HOME/bin/fem_to_gmsh mesh3d >> fem_to_gmsh_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
$HOME/bin/fem_to_gmsh rectangle >> fem_to_gmsh_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
