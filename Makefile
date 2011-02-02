GPRBUILD=gprbuild
GPRCLEAN=gprclean
TESTRUNNER=testrunner

lib:
	mkdir -p build
	$(GPRBUILD) -p epoll.gpr

syntax:
	mkdir -p build
	gnatmake -gnatc -gnat05 -P epoll.gpr

clean:
	$(GPRCLEAN) epoll.gpr
	rm -rf build

.PHONY: syntax lib
