#! /bin/bash
#
echo "Estimate volume of 6D sphere using sparse grid rules."
#
~/bin/hyperball_volume_quad ccl_d6_level0 >  hyperball_volume_quad_test.txt
~/bin/hyperball_volume_quad ccl_d6_level1 >> hyperball_volume_quad_test.txt
~/bin/hyperball_volume_quad ccl_d6_level2 >> hyperball_volume_quad_test.txt
~/bin/hyperball_volume_quad ccl_d6_level3 >> hyperball_volume_quad_test.txt
~/bin/hyperball_volume_quad ccl_d6_level4 >> hyperball_volume_quad_test.txt
~/bin/hyperball_volume_quad ccl_d6_level5 >> hyperball_volume_quad_test.txt
#
echo "Estimate volume of 6D sphere using equivalent Monte Carlo rules."
#
~/bin/hyperball_volume_quad uniform_d6_00001a >> hyperball_volume_quad_test.txt
~/bin/hyperball_volume_quad uniform_d6_00013a >> hyperball_volume_quad_test.txt
~/bin/hyperball_volume_quad uniform_d6_00085a >> hyperball_volume_quad_test.txt
~/bin/hyperball_volume_quad uniform_d6_00389a >> hyperball_volume_quad_test.txt
~/bin/hyperball_volume_quad uniform_d6_01457a >> hyperball_volume_quad_test.txt
~/bin/hyperball_volume_quad uniform_d6_04865a >> hyperball_volume_quad_test.txt
#
echo " "
echo "(Moral: do not use interpolatory rules on discontinuous data.)"
echo " "
echo "Normal end of execution."

