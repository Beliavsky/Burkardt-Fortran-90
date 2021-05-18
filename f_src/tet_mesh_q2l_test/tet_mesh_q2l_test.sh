#! /bin/bash
#
$HOME/bin/tet_mesh_q2l cube > tet_mesh_q2l_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
