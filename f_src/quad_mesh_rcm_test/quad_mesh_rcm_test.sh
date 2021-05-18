#! /bin/bash
#
$HOME/bin/quad_mesh_rcm hole > quad_mesh_rcm_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
