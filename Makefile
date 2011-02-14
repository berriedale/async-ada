GPRBUILD=gprbuild
GPRCLEAN=gprclean
TESTRUNNER=testrunner

PROJECTGPR=async.gpr

lib: pre
	$(GPRBUILD) -p $(PROJECTGPR)

syntax: pre
	gnatmake -gnatc -gnat05 -P $(PROJECTGPR)

clean: pre
	for d in tests/*; do echo "> $$d"; (cd $$d && make clean); done
	$(GPRCLEAN) $(PROJECTGPR)
	rm -rf build

pre:
	mkdir -p build

test: pre lib
	for d in tests/*; do echo "> $$d"; (cd $$d && make run); done

.PHONY: syntax lib test
