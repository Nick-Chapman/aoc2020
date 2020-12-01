
#include <stdio.h>
#include <fstream>

int main() {

  // How many numbers in the input file?
  int size = 0;
  {
    std::ifstream inp("../input/day1.input");
    int q;
    while (inp >> q) size++;
  }
  printf("size=%d\n",size); // without this line we seg-error. why???

  // Read them in...
  int* input = new int [size];
  {
    std::ifstream inp("../input/day1.input");
    int i = 0;
    while (inp >> input[i++]);
  }

  // Set up an array of bools, which are true when the index is a number in the input
  bool one[2021] = {};
  for (int i = 0; int x = input[i]; i++) { // order N
    one[x] = true;
  }

  // Iterating over the inputs & checking the one array, compute part1
  for (int i = 0; int x = input[i]; i++) { // order N
    int y = 2020 - x;
    if (y>=0 && one[y]) {
      printf("part1 -> %d\n", x*y);
      break;
    }
  }

  // Set up an array of bools, which will be true when two numbers in the input sum to that index
  bool two[2021] = {};

  // And record the multiple of the numbers which sum thus.
  int two_mul[2021] = {};

  for (int a = 0; a <= 2020; a++) { // order k = 2020 (independent of input size)
    for (int i = 0; int x = input[i]; i++) { // order N
      int y = a - x;
      if (y>=0 && one[y]) {
        two[a] = true;
        two_mul[a] = x * y;
        break;
      }
    }
  }

  // Iterating over the inputs & checking the two array, compute part2
  for (int i = 0; int x = input[i]; i++) { // order N
    int y = 2020 - x;
    if (y>=0 && two[y]) {
      int m = two_mul[y];
      printf("part2 -> %d \n", x * m);
      break;
    }
  }

}
