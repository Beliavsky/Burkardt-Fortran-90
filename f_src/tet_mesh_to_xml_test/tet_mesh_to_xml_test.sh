#! /bin/bash
#
$HOME/bin/tet_mesh_to_xml mesh > tet_mesh_to_xml_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
