#include <stdio.h>

int fib(int n) {
  if (n < 2) return 1;
  return fib(n-1) + fib(n-2);
}

void show(int n) {
  printf("%d: %d\n", n, fib(n));
}

int main(int argc, char **argv) {
  show(35);
  return 0;
}
