scheme = guile -s
all: test
test: a.out
	./a.out
a.out: tmp.s
	$(CC) $<
tmp.s: compiler.scm
	$(scheme) $< > $@
	mv $@.ref $@.ref.old ||:
	cp $@ $@.ref
	diff -u $@.ref.old $@.ref ||:
clean:
	rm -f a.out tmp.s
