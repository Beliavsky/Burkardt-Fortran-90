#! /bin/bash
#
$HOME/bin/tet_mesh_l2q mesh > tet_mesh_l2q_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
