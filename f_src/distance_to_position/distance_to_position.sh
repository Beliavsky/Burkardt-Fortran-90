#! /bin/bash
#
gfortran -c -Wall distance_to_position.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran distance_to_position.o $HOME/lib/nms.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm distance_to_position.o
#
chmod ugo+x a.out
mv a.out ~/bin/distance_to_position
#
echo "Normal end of execution."
