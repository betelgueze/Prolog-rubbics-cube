all:
	swipl -q -g start -o rubick -c rubick.pl
    
zip:
	zip flp-log-xrisam00.zip rubick.pl Makefile README ./tests/* run-tests.sh
    
clean:
	rm -f rubick

test:
	swipl -q -g start -o rubick -c rubick.pl
	./run-tests.sh
	rm -f rubick