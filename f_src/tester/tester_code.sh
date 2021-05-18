#! /bin/bash
#
cd ..
for TEST in `ls -d c*_test`;
do
  if [ -d $TEST ]
  then
    echo $TEST
    cd $TEST
    time ./$TEST.sh
    cd ..
  fi
done
#
echo ""
echo "tester_code.sh:"
echo "  Normal end of execution."
