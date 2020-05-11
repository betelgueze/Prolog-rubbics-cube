#!/bin/ksh

green='\e[1;32m%s\e[0m\n'
red='\e[1;31m%s\e[0m\n'

mkdir -p tmp && rm -rf tmp/*

./rubick < "tests/0step_solved_rubick.txt" >tmp/"solved".steps 2>/dev/null
./rubick < "tests/1step_left_rotation.txt" >tmp/"1step".steps 2>/dev/null
./rubick < "tests/2step_mid_rotation.txt" >tmp/"2step".steps 2>/dev/null

diff --strip-trailing-cr -u "tests/0step.steps" "tmp/solved.steps" >/dev/null 2>&1
if [ $? -eq 0 ]; then
	printf "$green" "Test 01 - 0 step PASS"
else
	printf "$red" "Test 01 - 0 step FAIL"
fi

diff --strip-trailing-cr -u "tests/1step.steps" "tmp/1step.steps" >/dev/null 2>&1
if [ $? -eq 0 ]; then
	printf "$green" "Test 02 - 1 step PASS"
else
	printf "$red" "Test 02 - 1 step FAIL"
fi

diff --strip-trailing-cr -u "tests/2step.steps" "tmp/2step.steps" >/dev/null 2>&1
if [ $? -eq 0 ]; then
	printf "$green" "Test 03 - 2 step PASS"
else
	printf "$red" "Test 03 - 2 step FAIL"
fi

rm -rf tmp