#! /bin/bash
#
gfortran -c pyramid_rule.f90
if [ $? -ne 0 ]; then
  echo "Errors compiling pyramid_rule.f90"
  exit
fi
#
gfortran pyramid_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm pyramid_rule.o
#
chmod ugo+x a.out
mv a.out ~/bin/pyramid_rule
#
echo "Normal end of execution."
