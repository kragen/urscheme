scheme = guile -s
all: test
test: a.out
	./a.out
a.out: tmp.s
	$(CC) $<
tmp.s: compiler.scm
	$(scheme) $< > $@
clean:
	rm -f a.out tmp.s
