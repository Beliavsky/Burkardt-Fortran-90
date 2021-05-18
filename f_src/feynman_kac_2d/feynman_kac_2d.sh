#! /bin/bash
#
gfortran -c -Wall feynman_kac_2d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran feynman_kac_2d.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading feynman_kac_2d.o"
  exit
fi
rm feynman_kac_2d.o
#
chmod ugo+x a.out
mv a.out $HOME/bin/feynman_kac_2d
#
echo "Normal end of execution."
