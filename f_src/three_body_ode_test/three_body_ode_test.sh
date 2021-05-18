#! /bin/bash
#
gfortran -c -Wall three_body_ode_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran three_body_ode_test.o $HOME/lib/three_body_ode.o $HOME/lib/rkf45.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm three_body_ode_test.o
#
mv a.out three_body_ode_test
./three_body_ode_test > three_body_ode_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm three_body_ode_test
#
echo "Normal end of execution."
