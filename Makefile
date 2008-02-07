# Use any of these:
#scheme = guile -s
scheme = mzscheme -r
#scheme = scm -f
# Bigloo produces lots of warnings but works:
#scheme = bigloo -i
#scheme = elk -l
listing=tmp.s.lst

# Unfortunately RScheme's interpreter "rs" outputs warnings to stdout,
# and it emits a warning when we define the variable "text", so I'm
# not going to bother to make this work with RScheme.

# The code mostly works in tinyscheme, but apparently in tinyscheme,
# (read) reads from the program you're running, not from stdin.  This
# means the compiler can't read its input, and actually it seems to
# prematurely exit with no error message when it tries.  It used to
# work before it started trying to read input...

# This -nostdlib flag is optional; it just makes a smaller output
# file.  (It's unusual to have an assembly file that can compile both
# with and without -nostdlib, but this is one.)
asflags = -nostdlib -Wa,-adhlns=$(listing)
all: test tests
test:
	./a.out
a.out: tmp.s
	$(CC) $(asflags) $<
tmp.s: compiler.scm test.simple.scm
	$(scheme) compiler.scm < test.simple.scm > $@
	mv $@.ref $@.ref.old ||:
	cp $@ $@.ref
	diff -u $@.ref.old $@.ref ||:
clean:
	rm -f a.out tmp.s $(listing) tmp.s.ref tmp.s.ref.old
tests:
	./runtests
