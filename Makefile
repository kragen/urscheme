# Use any of these:
#scheme = guile -s
scheme = mzscheme -r
#scheme = tinyscheme
#scheme = scm -f
#scheme = bigloo -i
#scheme = elk -l

# Unfortunately RScheme's interpreter "rs" outputs warnings to stdout,
# and it emits a warning when we define the variable "text", so I'm
# not going to bother to make this work with RScheme.

# This -nostdlib flag is optional; it just makes a smaller output
# file.  (It's unusual to have an assembly file that can compile both
# with and without -nostdlib, but this is one.)
asflags = -nostdlib -Wa,-adhlns=$<.lst
all: test
test: a.out
	./a.out
a.out: tmp.s
	$(CC) $(asflags) $<
tmp.s: compiler.scm
	$(scheme) $< > $@
	mv $@.ref $@.ref.old ||:
	cp $@ $@.ref
	diff -u $@.ref.old $@.ref ||:
clean:
	rm -f a.out tmp.s
