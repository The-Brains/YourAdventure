# Git tags
GIT_TAG?=build
GIT_SHA=$(shell git rev-parse HEAD | cut -c1-7)

# dockers
PROJECT_NAME:=$$(cat ./PROJECT_NAME)
DOCKER_BINARY_PATH:=./target/docker/0/$(PROJECT_NAME)-$(VERSION)-all.jar

VERSION=$$(cat VERSION)

### CLEAN ###

deep_clean:
	rm -fr **/target
	rm -fr ./target
	rm -fr ./.ivy2
	make clean

clean:
	rm $(BINARY_PATH) || echo ""
	sbt clean cleanFiles

help:
	(ls $(DOCKER_BINARY_PATH) || make docker_build)
	scala $(BINARY_PATH) --help

### DOCKER ###

package:
	export SBT_OPTS="-Xmx2G" ; sbt 'set test in assembly := {}' docker

### TEST ###

test:
	sbt clean test

test_coverage:
	sbt clean coverage test coverageReport
	open ./target/scala-2.12/scoverage-report/index.html

#  make SEARCH="MY TEST NAME" test_only
test_only:
	sbt "testOnly -- -z ${SEARCH}"

# make SEARCH="MyClassTest" test_only
test_class:
	sbt "testOnly *${SEARCH}"
