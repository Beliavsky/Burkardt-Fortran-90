#! /bin/bash
#
$HOME/bin/truncated_normal_rule < option0_input.txt >  truncated_normal_rule_test.txt
$HOME/bin/truncated_normal_rule < option1_input.txt >> truncated_normal_rule_test.txt
$HOME/bin/truncated_normal_rule < option2_input.txt >> truncated_normal_rule_test.txt
$HOME/bin/truncated_normal_rule < option3_input.txt >> truncated_normal_rule_test.txt
#
echo "Normal end of execution."

