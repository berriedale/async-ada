GPRBUILD=gprbuild
GPRCLEAN=gprclean
TESTRUNNER=testrunner

PROJECTGPR=async.gpr

release: pre syntax
	$(GPRBUILD) -p $(PROJECTGPR) -Xmode=release

lib: pre
	$(GPRBUILD) -p $(PROJECTGPR)

syntax: pre lib
	gnatmake -gnatc -gnat05 -P $(PROJECTGPR)

clean: pre
	for d in tests/*; do echo "> $$d"; (cd $$d && make clean); done
	$(GPRCLEAN) $(PROJECTGPR)
	rm -rf obj

pre:
	mkdir -p obj/debug obj/release

test: pre lib
	for d in tests/*; do echo "> $$d"; (cd $$d && make run); done

.PHONY: syntax lib test
