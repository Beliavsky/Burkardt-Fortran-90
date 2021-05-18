#! /bin/bash
#
cat <<EOF > input.txt
2000
1
EOF
#
$HOME/bin/ranmap < input.txt > ranmap_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
ps2png ranmap.eps cross.png
#
cat <<EOF > input.txt
2000
2
EOF
#
$HOME/bin/ranmap < input.txt >> ranmap_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
ps2png ranmap.eps dragon.png
#
cat <<EOF > input.txt
2000
3
EOF
#
$HOME/bin/ranmap < input.txt >> ranmap_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
ps2png ranmap.eps fern.png
#
cat <<EOF > input.txt
2000
4
EOF
#
$HOME/bin/ranmap < input.txt >> ranmap_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
ps2png ranmap.eps leaf.png
#
cat <<EOF > input.txt
2000
5
EOF
#
$HOME/bin/ranmap < input.txt >> ranmap_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
ps2png ranmap.eps levy_dragon.png
#
cat <<EOF > input.txt
2000
6
EOF
#
$HOME/bin/ranmap < input.txt >> ranmap_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
ps2png ranmap.eps tree.png
#
cat <<EOF > input.txt
2000
7
EOF
#
$HOME/bin/ranmap < input.txt >> ranmap_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
ps2png ranmap.eps triangle.png
#
echo "Normal end of execution."
