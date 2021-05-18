#! /bin/bash
#
gfortran -c -Wall football_scores.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o football_scores football_scores.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm football_scores.o
#
mv football_scores ~/bin/football_scores
#
echo "Normal end of execution."
