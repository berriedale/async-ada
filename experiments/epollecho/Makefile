GPRBUILD=gprbuild
GPRCLEAN=gprclean

PROJECTGPR=echoserver.gpr

build: pre
	$(GPRBUILD) -p $(PROJECTGPR)

syntax: pre lib
	gnatmake -gnatc -gnat05 -P $(PROJECTGPR)

clean: pre
	$(GPRCLEAN) $(PROJECTGPR)
	rm -rf ob

pre:
	mkdir -p obj

.PHONY: syntax build
