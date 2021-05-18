#! /bin/bash
#
$HOME/bin/tet_mesh_to_gmsh mesh > tet_mesh_to_gmsh_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
