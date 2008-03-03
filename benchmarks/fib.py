#!/usr/bin/python
def fib(n):
    if n < 2: return 1
    return fib(n-1) + fib(n-2)
def show(n): print '%d: %d' % (n, fib(n))
if __name__ == '__main__': show(30)
