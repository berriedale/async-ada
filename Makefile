GPRBUILD=gprbuild
GPRCLEAN=gprclean
TESTRUNNER=testrunner

lib: pre
	$(GPRBUILD) -p epoll.gpr

syntax: pre
	gnatmake -gnatc -gnat05 -P epoll.gpr

clean: pre
	for d in tests/*; do echo "> $$d"; (cd $$d && make clean); done
	$(GPRCLEAN) epoll.gpr
	rm -rf build

pre:
	mkdir -p build

test: pre lib
	for d in tests/*; do echo "> $$d"; (cd $$d && make run); done

.PHONY: syntax lib test
