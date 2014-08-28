.PHONY: all
all:test

.PHONY: test
test :mk.rkt test/unit-test.scm
	racket test/unit-test.scm

test/unit-test.scm : test/unit-test-header.scm test/common-test.scm test/matchee-test.scm
	cat test/unit-test-header.scm test/common-test.scm test/matchee-test.scm > test/unit-test.scm


