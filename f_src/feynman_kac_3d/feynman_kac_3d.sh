#! /bin/bash
#
gfortran -c -Wall feynman_kac_3d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran feynman_kac_3d.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm feynman_kac_3d.o
#
chmod ugo+x a.out
mv a.out $HOME/bin/feynman_kac_3d
#
echo "Normal end of execution."
