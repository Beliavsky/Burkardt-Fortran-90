#! /bin/bash
#
$HOME/bin/tetrahedron_properties tet_cap.txt > tetrahedron_properties_test.txt
$HOME/bin/tetrahedron_properties tet_equilateral.txt >> tetrahedron_properties_test.txt
$HOME/bin/tetrahedron_properties tet_needle.txt >> tetrahedron_properties_test.txt
$HOME/bin/tetrahedron_properties tet_right.txt >> tetrahedron_properties_test.txt
$HOME/bin/tetrahedron_properties tet_sliver.txt >> tetrahedron_properties_test.txt
$HOME/bin/tetrahedron_properties tet_spindle.txt >> tetrahedron_properties_test.txt
$HOME/bin/tetrahedron_properties tet_wedge.txt >> tetrahedron_properties_test.txt
#
echo "Normal end of execution."

