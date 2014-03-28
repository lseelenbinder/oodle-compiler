#include <syscall.h>

void writeint(int num) {
  char buf[20];
  char result[20] = "0\n";
  char *pos = buf;
  char *writeptr = result;
  int numWritten;

  // Handle negative numbers
  if (num < 0) {
    *writeptr++ = '-';
    num = -num;
  }

  if (num > 0) {

    // Build the number in reverse order
    while (num > 0) {
      *pos++ = (num % 10) + '0';
      num /= 10;
    }
    pos--;

    // Now we need to copy the results into the output buffer, reversed
    while (pos > buf) {
      *writeptr++ = *pos--;
    }
    *writeptr++ = *pos;
    *writeptr++ = 10;
    *writeptr++ = 0;
  } else {
    // number is 0; use default result
    writeptr = result + 3;
  }

  write(1, result, (writeptr - result) - 1);

}

int power(int n, int p) {
  int i = 0, sum = 1;
  for (i; i < p; ++i) sum *= n;
  return sum;
}

int readint() {
  char buf[20];
  int i, sum;
  char *end;
  char *pos = buf;
  int neg = 0;

  for (i = 0; i < 20; i++) buf[i] = 0;

  read(0, buf, 19);

  for (i = 0; i < 20; i++) {
    if (buf[i] == 10) {
      end = &buf[i-1];
    }
  }

  if (buf[0] == '-') {
    neg = 1;
    ++pos;
  }

  sum = 0;
  i = end - pos;
  while (pos <= end) {
    sum += (0x30 ^ *pos) * power(10, i);
    ++pos;
    --i;
  }

  if (neg) sum = -sum;

  return sum;
}
