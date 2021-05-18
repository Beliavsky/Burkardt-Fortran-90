#! /bin/bash
#
$HOME/bin/tet_mesh_quality cube > tet_mesh_quality_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
