#! /bin/bash
#
$HOME/bin/tet_mesh_quad cube_4x4x4 > tet_mesh_quad_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
