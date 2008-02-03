scheme = guile -s
all: test
test: a.out
	./a.out
a.out: tmp.s
	$(CC) $<
tmp.s: compiler.scm
	mv $@ $@.old ||:
	$(scheme) $< > $@
	diff -u $@.old $@ ||:
clean:
	rm -f a.out tmp.s
