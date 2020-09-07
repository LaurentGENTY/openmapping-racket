all: clean test

test: test-all test-path$

test-all: test/all-tests.rkt
	racket test/all-tests.rkt

test-path: test/test-paths.rkt
	racket test/test-paths.rkt

clean:
	rm -f src/*.bak test/*.bak
	rm -f *~ src/*~ test/*~
