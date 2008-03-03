# Use any of these:
#scheme = guile -s
scheme = mzscheme -M errortrace -r
# SCM gives nice stack traces:
#scheme = scm -f
# Bigloo produces lots of warnings but used to work, but it demands
# that you pass only a single argument to (error ...)
#scheme = bigloo -i
#scheme = elk -l
# The Chicken Scheme interpreter:
#scheme = csi -s
# Stalin doesn't work because it doesn't recognize #\tab, but I seem
# to recall that it had some deeper problem as well.
listing=tmp.s.lst

# Unfortunately RScheme's interpreter "rs" outputs warnings to stdout,
# and it emits a warning when we define the variable "text", so I'm
# not going to bother to make this work with RScheme.

# The code used to mostly work in tinyscheme, but apparently in
# tinyscheme, (read) reads from the program you're running, not from
# stdin.  This means the compiler can't read its input, and actually
# it seems to prematurely exit with no error message when it tries.
# It used to work before it started trying to read input... but now it
# has another problem as well when it starts trying to compile the
# standard library: "Error: xxx->string: not a xxx #f".  Dunno what
# that's about.

# This -nostdlib flag is optional; it just makes a smaller output
# file.  (It's unusual to have an assembly file that can compile both
# with and without -nostdlib, but this is one.)
#asflags = -nostdlib
# If you're using gas, to generate an assembly listing:
asflags = -nostdlib -Wa,-adhlns=$(listing)
all: test tests urscheme-compiler
urscheme-compiler: compiler.s
	time $(CC) $(asflags) $< -o $@
compiler.s: compiler.scm
	time $(scheme) $< < $< > $@
test: a.out
	./a.out
a.out: tmp.s
	$(CC) $(asflags) $<
tmp.s: compiler.scm test.crufty.scm
	$(scheme) compiler.scm < test.crufty.scm > $@
	mv $@.ref $@.ref.old ||:
	cp $@ $@.ref
	diff -u $@.ref.old $@.ref ||:
clean:
	rm -f a.out tmp.s $(listing) tmp.s.ref tmp.s.ref.old \
		urscheme-compiler compiler.s runscheme.s compiler.s.stage2 \
		compiler.c compiler_c compiler.gambit.s compiler.so \
		compiler.gambit.s compiler.mzc.s
slowtests: chmodding
	./runtests
	./test-read-char
tests: chmodding urscheme-compiler
	urscheme=./urscheme-compiler ./runtests
	./test-read-char
summary:
	egrep '^;;;|^\(' compiler.scm
chmodding:
	chmod 755 runtests runscheme test-read-char
stage2-test: urscheme-compiler compiler.s compiler.scm
	time ./urscheme-compiler < compiler.scm > compiler.s.stage2
	diff -u compiler.s compiler.s.stage2
# Rule to compile with Gambit.
# The Gambit documentation warns that -O -D___SINGLE_HOST may use too
# much memory or take too long, but on my laptop it only takes two
# minutes to compile, and produces a faster executable.
gambit-test: compiler.s
	time gsc -link compiler.scm
	time $(CC) -O -D___SINGLE_HOST compiler.c compiler_.c -lgambc -o gambit-compiler
	time ./gambit-compiler < compiler.scm > compiler.gambit.s
	diff -u compiler.s compiler.gambit.s
# mzc does work, but it's even slower than the MzScheme interpreter
# --- twice as slow!  And it takes even longer than Gambit-C to compile.
compiler.so: compiler.scm
	time mzc compiler.scm
mzc-test: compiler.s compiler.so
	time mzscheme -mve '(load-extension "compiler.so")' < compiler.scm > compiler.mzc.s
	diff -u compiler.mzc.s compiler.s
chicken-test: compiler.s
	time csc compiler.scm
	mv compiler chicken-compiler
	time ./chicken-compiler < compiler.scm > compiler.chicken.s
	diff -u compiler.s compiler.chicken.s
