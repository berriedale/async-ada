

all: pre
	gprbuild -P test.gpr

run: all
	./epoll-test

clean: pre
	gprclean -P test.gpr

pre:
	mkdir -p build
