#! /bin/bash
#
#  Discard the files we made on a previous run.
#
if [ -f "alpha.f90" ]; then
  rm alpha.f90
fi
if [ -f "beta.f90" ]; then
  rm beta.f90
fi
if [ -f "blockdata.f90" ]; then
  rm blockdata.f90
fi
if [ -f "delta.f90" ]; then
  rm delta.f90
fi
if [ -f "enid.f90" ]; then
  rm enid.f90
fi
if [ -f "epsilon.f90" ]; then
  rm epsilon.f90
fi
if [ -f "eta.f90" ]; then
  rm eta.f90
fi
if [ -f "gamma.f90" ]; then
  rm gamma.f90
fi
if [ -f "iota.f90" ]; then
  rm iota.f90
fi
if [ -f "no_name.f90" ]; then
  rm no_name.f90
fi
if [ -f "rho.f90" ]; then
  rm rho.f90
fi
if [ -f "theta.f90" ]; then
  rm theta.f90
fi
if [ -f "zeta.f90" ]; then
  rm zeta.f90
fi
#
~/bin/fsplit -v < fsplit_test.f90 > fsplit_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
