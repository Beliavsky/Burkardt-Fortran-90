#! /bin/bash
#
$HOME/bin/fd_predator_prey 100 > fd_predator_prey_test.txt
gnuplot < fd_predator_prey_commands.txt
mv fd_predator_prey.png fd_predator_prey_100.png
#
$HOME/bin/fd_predator_prey 1000 >> fd_predator_prey_test.txt
gnuplot < fd_predator_prey_commands.txt
mv fd_predator_prey.png fd_predator_prey_1000.png
#
$HOME/bin/fd_predator_prey 10000 >> fd_predator_prey_test.txt
gnuplot < fd_predator_prey_commands.txt
mv fd_predator_prey.png fd_predator_prey_10000.png
#
echo "Normal end of execution."
