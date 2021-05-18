#! /bin/bash
#
gfortran -c -Wall cellular_automaton.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran cellular_automaton.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cellular_automaton.o
#
chmod ugo+x a.out
mv a.out ~/bin/cellular_automaton
#
echo "Normal end of execution."
