#! /bin/bash
#
rm -f plot_to_ps_test.txt
#
$HOME/bin/plot_to_ps worms.plot >> plot_to_ps_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
