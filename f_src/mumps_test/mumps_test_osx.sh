#! /bin/bash
#
gfortran -c -Wall -I /usr/local/include -I $HOME/include mumps_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o mumps_test -O mumps_test.o  -L/usr/local/lib -ldmumps \
  -lmumps_common -lpord -L/usr/lib  -lmetis -L/opt/local/lib -lesmumps \
  -lscotch -lscotcherr -llapack -lmpiseq -lblas -lpthread
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm mumps_test.o
#
./mumps_test < mumps_test_input.txt > mumps_test_osx.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm mumps_test
#
echo "Normal end of execution."


